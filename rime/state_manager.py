import fcntl
import json
import os
import shutil
import stat
import subprocess
import sys
from contextlib import contextmanager
from pathlib import Path

import dotfiles_files
from dotfiles_files import copy_file, fail, same_content, write_text


def lexists(path: Path) -> bool:
    return os.path.lexists(path)


def state_paths() -> tuple[Path, Path, Path, Path]:
    data_dir = dotfiles_files.data_home() / "fcitx5/rime"
    state_dir = dotfiles_files.state_home() / "rime"
    return (
        data_dir,
        data_dir / ".home-manager-static",
        state_dir,
        state_dir / "home-manager-source-stamp",
    )


def validate_regular(path: Path, description: str) -> None:
    if path.is_symlink() or (path.exists() and not path.is_file()):
        fail(f"Refusing malformed Rime {description}: {path}")


def validate_directory(directory: Path) -> None:
    if directory.is_symlink() or (
        directory.exists() and not directory.is_dir()
    ):
        fail(f"Refusing malformed Rime path: {directory}")


def staging(static_dir: Path) -> Path:
    # Stay on the live tree's filesystem for rename. The old random siblings
    # have no ownership record; even a name shaped like mkdtemp's may be a
    # user backup. Never enumerate or migrate them.
    return static_dir.parent / ".home-manager-rime-work" / "incoming"


def validate_workspace(incoming: Path) -> None:
    workspace = incoming.parent
    validate_directory(workspace)
    if not workspace.exists():
        return
    marker = workspace / "owner-v1"
    validate_regular(marker, "staging marker")
    if marker.exists() and marker.stat().st_size != 0:
        fail(f"Refusing malformed Rime staging marker: {marker}")
    if not marker.exists() and any(workspace.iterdir()):
        fail(f"Refusing unmarked Rime staging directory: {workspace}")
    validate_directory(incoming)
    validate_directory(workspace / "previous")


def managed_destination(link: Path, static_dir: Path) -> Path | None:
    # Inspect the immediate link, not a chain through someone else's link.
    # Accept a direct relative spelling as well as our own absolute one.
    raw = link.readlink()
    dest = Path(os.path.normpath(link.parent / raw))
    if dest.is_relative_to(static_dir) and (
        raw == dest or raw == Path(os.path.relpath(dest, link.parent))
    ):
        return dest
    return None


def unmanaged_parent(root: Path, relative: Path) -> Path | None:
    directory = root
    for name in relative.parts[:-1]:
        directory /= name
        if directory.is_symlink() or (
            directory.exists() and not directory.is_dir()
        ):
            return directory
    return None


def valid_name(name: str) -> bool:
    path = Path(name)
    return (
        bool(path.parts)
        and not path.is_absolute()
        and str(path) == name
        and "\0" not in name
        and not any(part.startswith(".") for part in path.parts)
    )


def declared_links(manifest: Path) -> set[str] | None:
    validate_regular(manifest, "link manifest")
    if not manifest.exists():
        return None
    try:
        names = json.loads(manifest.read_text(encoding="utf-8"))
        if isinstance(names, list) and all(
            isinstance(name, str) and valid_name(name) for name in names
        ):
            return set(names)
    except (UnicodeError, ValueError):
        pass
    fail(f"Refusing malformed Rime link manifest: {manifest}")


def legacy_names(tree: Path) -> set[str]:
    # Before there was a manifest only surviving payloads in an owned tree
    # supply evidence. Never search the user's data directory for aliases.
    return {
        name
        for directory, _, files in os.walk(tree, followlinks=False)
        for file in files
        if valid_name(name := str((Path(directory) / file).relative_to(tree)))
    }


def retire_link(static_dir: Path, relative: Path) -> None:
    data_dir = static_dir.parent
    if unmanaged_parent(data_dir, relative):
        return
    link = data_dir / relative
    if link.is_symlink() and managed_destination(link, static_dir) == (
        static_dir / relative
    ):
        link.unlink()


def make_user_writable(root: Path) -> None:
    for directory, _, files in os.walk(root):
        path = Path(directory)
        path.chmod(stat.S_IMODE(path.stat().st_mode) | stat.S_IWUSR)
        for name in files:
            file_path = path / name
            file_path.chmod(
                stat.S_IMODE(file_path.stat().st_mode) | stat.S_IWUSR
            )


def remove_staging(root: Path) -> None:
    """Remove a staging tree, including one whose copy never finished.

    copytree gives each directory its source's mode once that directory's
    contents are copied. The source is a read-only store path, so an
    interrupted copy leaves directories that cannot be emptied as they are.
    """
    for _, _, _, fd in os.fwalk(root, follow_symlinks=False):
        os.fchmod(fd, stat.S_IMODE(os.fstat(fd).st_mode) | stat.S_IRWXU)
    shutil.rmtree(root)


def refresh_static(
    static_source: Path, static_dir: Path, backup: Path, stamp: Path
) -> None:
    temporary = staging(static_dir)
    workspace = temporary.parent
    workspace.mkdir(mode=0o700, exist_ok=True)
    marker = workspace / "owner-v1"
    if not marker.exists():
        marker.touch(mode=0o600, exist_ok=False)
    moved = False
    try:
        shutil.copytree(
            static_source,
            temporary,
            symlinks=False,
        )
        make_user_writable(temporary)
        # With no old tree, a leftover stamp identifies nothing.
        if not static_dir.exists():
            stamp.unlink(missing_ok=True)
        if lexists(static_dir):
            # Keep the old tree for rollback if installation fails. The two
            # renames are not one atomic swap: static_dir is absent between
            # them, and a killed run relies on recovery at the next activation.
            os.replace(static_dir, backup)
            moved = True
        try:
            os.replace(temporary, static_dir)
        except OSError:
            if moved and not lexists(static_dir):
                os.replace(backup, static_dir)
            raise
        # Until both renames succeed the stamp still belongs to the backup.
        # Invalidate it before editing any visible links or deleting any
        # part of that backup. Publish the new one only after cleanup.
        stamp.unlink(missing_ok=True)
    finally:
        if temporary.exists():
            remove_staging(temporary)


def plan_deployment(
    static_source: Path,
    stamp_source: Path,
    relatives: list[str],
) -> tuple[Path, set[str] | None]:
    """Select the recovery tree and outgoing link names without writing.

    Activation runs this before Home Manager's write boundary, so a refusal
    stops it while the previous generation is still intact.
    """
    if not static_source.is_dir():
        fail(f"Rime static source is not a directory: {static_source}")
    if not stamp_source.is_file():
        fail(f"Rime stamp source is not a regular file: {stamp_source}")
    data_dir, static_dir, state_dir, stamp = state_paths()
    for directory in (data_dir.parent, data_dir, static_dir, state_dir):
        validate_directory(directory)
    incoming = staging(static_dir)
    validate_workspace(incoming)
    validate_regular(stamp, "source stamp")

    live = static_dir
    previous = incoming.with_name("previous")
    if previous.exists() and (not live.exists() or stamp.exists()):
        live = previous
    elif not live.exists():
        # An interrupted legacy swap and a personal backup have the same
        # unmarked name. Its contents cannot prove which one it is.
        legacy = static_dir.with_name(static_dir.name + ".home-manager-old")
        if lexists(legacy):
            fail(
                f"Refusing ambiguous legacy Rime backup: {legacy}. "
                "Inspect it and move it to an unused backup name, then "
                "activate again. Keep the backup for manual recovery."
            )
    for name in relatives:
        relative = Path(name)
        if not valid_name(name):
            fail(f"Invalid Rime static relative path: {name}")
        source = static_source / relative
        if not source.is_file():
            fail(f"Rime static source does not exist as a file: {source}")
        for root in (data_dir, live):
            parent = unmanaged_parent(root, relative)
            if parent is not None:
                fail(f"Refusing unmanaged Rime path: {parent}")
        validate_regular(live / relative, "materialized file")
        target = data_dir / relative
        if target.is_symlink():
            if managed_destination(target, static_dir) is None:
                fail(f"Refusing to replace unmanaged Rime link: {target}")
        elif target.exists():
            fail(f"Refusing to replace unmanaged Rime path: {target}")
    return live, declared_links(state_dir / "home-manager-schema-links.json")


@contextmanager
def lock_directory(directory: Path):
    # Two activations must not recover or remove each other's staging tree.
    fd = os.open(directory, os.O_DIRECTORY | os.O_NOFOLLOW)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        yield
    finally:
        os.close(fd)


def deploy(
    static_source: Path,
    stamp_source: Path,
    busctl: str,
    relatives: list[str],
) -> None:
    data_dir, static_dir, state_dir, stamp = state_paths()
    validate_directory(data_dir.parent)
    validate_directory(data_dir)
    if not data_dir.is_dir():
        plan_deployment(static_source, stamp_source, relatives)
        data_dir.mkdir(parents=True, exist_ok=True)
    with lock_directory(data_dir):
        live, recorded = plan_deployment(
            static_source, stamp_source, relatives
        )
        temporary = staging(static_dir)
        backup = temporary.with_name("previous")
        old = recorded
        if old is None:
            old = legacy_names(live) | legacy_names(backup)
        wanted = set(relatives)
        obsolete = old - wanted
        if temporary.exists():
            remove_staging(temporary)
        if live != static_dir:
            # The old stamp identifies the backup, not an interrupted
            # replacement. Its writable edits survive a same-source retry.
            if static_dir.exists():
                remove_staging(static_dir)
            os.replace(live, static_dir)
        elif backup.exists():
            # Installation succeeded; this backup is no longer the live tree.
            remove_staging(backup)
        refresh = not static_dir.is_dir() or not same_content(
            stamp_source, stamp
        )
        if refresh:
            print("Refreshing generated Rime schemas...")
            refresh_static(static_source, static_dir, backup, stamp)

        missing = [
            name for name in relatives if not (static_dir / name).exists()
        ]
        links = [
            name
            for name in relatives
            if not (data_dir / name).is_symlink()
            or (data_dir / name).readlink() != static_dir / name
        ]
        changed = bool(refresh or missing or links or obsolete)

        # Invalidate before repairing: even an interrupted repair cannot
        # leave the old compiled schema looking current on the next run.
        if changed:
            build = data_dir / "build"
            if build.is_symlink() or build.is_file():
                build.unlink()
            elif build.is_dir():
                shutil.rmtree(build)
        for name in obsolete:
            retire_link(static_dir, Path(name))
        # Forget an outgoing name only after retiring its exact link; record
        # incoming names before installing links. A killed run is retryable
        # on either side of the atomic manifest replacement.
        if recorded != wanted:
            write_text(
                state_dir / "home-manager-schema-links.json",
                json.dumps(sorted(wanted)) + "\n",
                0o600,
            )
        if not changed:
            return
        for name in missing:
            copy_file(static_source / name, static_dir / name, 0o644)
        for name in links:
            target = data_dir / name
            parent = unmanaged_parent(data_dir, Path(name))
            if parent is not None:
                fail(f"Refusing unmanaged Rime path: {parent}")
            target.parent.mkdir(parents=True, exist_ok=True)
            # Copying a generation can take time. Don't unlink a regular
            # file or a foreign link installed since preflight.
            if target.is_symlink():
                if managed_destination(target, static_dir) is None:
                    fail(f"Refusing to replace unmanaged Rime link: {target}")
                target.unlink()
            try:
                target.symlink_to(static_dir / name)
            except FileExistsError:
                fail(f"Refusing to replace unmanaged Rime path: {target}")
        if backup.exists():
            remove_staging(backup)
        if refresh:
            state_dir.mkdir(parents=True, exist_ok=True)
            copy_file(stamp_source, stamp, 0o644)
        result = subprocess.run(
            [
                busctl,
                "--user",
                "--auto-start=no",
                "call",
                "org.fcitx.Fcitx5",
                "/controller",
                "org.fcitx.Fcitx.Controller1",
                "ReloadAddonConfig",
                "s",
                "rime",
            ],
            check=False,
        )
        if result.returncode != 0:
            print(
                "Rime will rebuild generated schemas when Fcitx starts.",
                file=sys.stderr,
            )


def main(arguments: list[str]) -> None:
    if len(arguments) >= 5 and arguments[0] == "deploy":
        deploy(
            Path(arguments[1]),
            Path(arguments[2]),
            arguments[3],
            arguments[4:],
        )
        return
    if len(arguments) >= 4 and arguments[0] == "validate":
        plan_deployment(Path(arguments[1]), Path(arguments[2]), arguments[3:])
        return
    raise SystemExit(
        "usage: rime-state-manager deploy STATIC STAMP BUSCTL RELATIVE...\n"
        "       rime-state-manager validate STATIC STAMP RELATIVE..."
    )


if __name__ == "__main__":
    main(sys.argv[1:])
