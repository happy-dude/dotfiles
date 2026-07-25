import filecmp
import os
import shutil
import stat
import subprocess
import sys
import tempfile
from pathlib import Path

import dotfiles_files
from dotfiles_files import copy_file, fail


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


def iter_symlinks(root: Path, excluded: Path) -> list[Path]:
    links: list[Path] = []
    if not root.is_dir():
        return links
    for directory, names, files in os.walk(root, followlinks=False):
        current = Path(directory)
        retained_names: list[str] = []
        for name in names:
            candidate = current / name
            if candidate == excluded:
                continue
            if candidate.is_symlink():
                links.append(candidate)
            else:
                retained_names.append(name)
        names[:] = retained_names
        links.extend(
            current / name for name in files if (current / name).is_symlink()
        )
    return links


def resolves_below(path: Path, root: Path) -> bool:
    try:
        path.resolve(strict=False).relative_to(root.resolve(strict=False))
    except ValueError:
        return False
    return True


def validate_schema_target(
    target: Path, expected: Path, static_dir: Path
) -> None:
    if target.is_symlink():
        if target.resolve(strict=False) != expected.resolve(
            strict=False
        ) and not resolves_below(target, static_dir):
            fail(f"Refusing to replace unmanaged Rime link: {target}")
    elif target.exists():
        fail(f"Refusing to replace unmanaged Rime path: {target}")


def make_user_writable(root: Path) -> None:
    for directory, _, files in os.walk(root):
        path = Path(directory)
        path.chmod(stat.S_IMODE(path.stat().st_mode) | stat.S_IWUSR)
        for name in files:
            file_path = path / name
            file_path.chmod(
                stat.S_IMODE(file_path.stat().st_mode) | stat.S_IWUSR
            )


def refresh_static(static_source: Path, static_dir: Path) -> None:
    backup = static_dir.with_name(static_dir.name + ".home-manager-old")
    # Recover from a previous run that was killed mid-swap: the live tree was
    # moved aside but the replacement was never installed. Restore the backup
    # if the live tree is gone, otherwise discard a stale backup.
    if lexists(backup):
        if lexists(static_dir):
            shutil.rmtree(backup)
        else:
            os.replace(backup, static_dir)

    temporary = Path(
        tempfile.mkdtemp(dir=static_dir.parent, prefix=".home-manager-static.")
    )
    moved = False
    try:
        shutil.copytree(
            static_source,
            temporary,
            dirs_exist_ok=True,
            symlinks=False,
        )
        make_user_writable(temporary)
        if lexists(static_dir):
            # Move the live tree aside before installing the replacement so a
            # failure between the two never leaves Rime without its managed
            # data; the rename is atomic and the old tree can be rolled back.
            os.replace(static_dir, backup)
            moved = True
        try:
            os.replace(temporary, static_dir)
        except OSError:
            if moved and not lexists(static_dir):
                os.replace(backup, static_dir)
                moved = False
            raise
    finally:
        if temporary.exists():
            shutil.rmtree(temporary)
        if moved and lexists(backup):
            shutil.rmtree(backup)


def deploy(
    static_source: Path,
    stamp_source: Path,
    busctl: str,
    relatives: list[str],
) -> None:
    if not static_source.is_dir():
        fail(f"Rime static source is not a directory: {static_source}")
    if not stamp_source.is_file():
        fail(f"Rime stamp source is not a regular file: {stamp_source}")
    data_dir, static_dir, state_dir, stamp = state_paths()
    if static_dir.is_symlink() or (
        static_dir.exists() and not static_dir.is_dir()
    ):
        fail(f"Refusing malformed Rime static path: {static_dir}")
    validate_regular(stamp, "source stamp")

    for relative in relatives:
        validate_schema_target(
            data_dir / relative, static_dir / relative, static_dir
        )

    changed = (
        not static_dir.is_dir()
        or not stamp.exists()
        or not filecmp.cmp(stamp_source, stamp, shallow=False)
    )
    if changed:
        print("Refreshing generated Rime schemas...")
        data_dir.mkdir(parents=True, exist_ok=True)
        for link in iter_symlinks(data_dir, static_dir):
            if resolves_below(link, static_dir):
                link.unlink()
        refresh_static(static_source, static_dir)
        build = data_dir / "build"
        if build.is_symlink() or build.is_file():
            build.unlink()
        elif build.is_dir():
            shutil.rmtree(build)
        state_dir.mkdir(parents=True, exist_ok=True)
        copy_file(stamp_source, stamp, 0o644)

    for relative in relatives:
        source = static_dir / relative
        target = data_dir / relative
        if not source.exists():
            fail(f"Rime static source does not exist: {source}")
        target.parent.mkdir(parents=True, exist_ok=True)
        if not target.is_symlink():
            target.symlink_to(source)
            changed = True

    if changed:
        result = subprocess.run(
            [
                busctl,
                "--user",
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
    raise SystemExit(
        "usage: rime-state-manager deploy STATIC STAMP BUSCTL RELATIVE..."
    )


if __name__ == "__main__":
    main(sys.argv[1:])
