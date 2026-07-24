import filecmp
import os
import shutil
import stat
import subprocess
import sys
import tempfile
from pathlib import Path


def fail(message: str) -> None:
    raise SystemExit(message)


def lexists(path: Path) -> bool:
    return os.path.lexists(path)


def state_paths() -> tuple[Path, Path, Path, Path]:
    home = Path.home()
    data_home = Path(os.environ.get("XDG_DATA_HOME", home / ".local/share"))
    state_home = Path(os.environ.get("XDG_STATE_HOME", home / ".local/state"))
    data_dir = data_home / "fcitx5/rime"
    state_dir = state_home / "rime"
    return (
        data_dir,
        data_dir / ".home-manager-static",
        state_dir,
        state_dir / "home-manager-source-stamp",
    )


def validate_regular(path: Path, description: str) -> None:
    if path.is_symlink() or (path.exists() and not path.is_file()):
        fail(f"Refusing malformed Rime {description}: {path}")


def install_atomic(source: Path, target: Path, mode: int = 0o644) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        dir=target.parent, prefix=f".{target.name}."
    )
    temporary = Path(temporary_name)
    try:
        with (
            os.fdopen(descriptor, "wb") as output,
            source.open("rb") as input_file,
        ):
            shutil.copyfileobj(input_file, output)
            os.fchmod(output.fileno(), mode)
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, target)
        directory = os.open(target.parent, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    finally:
        temporary.unlink(missing_ok=True)


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
    temporary = Path(
        tempfile.mkdtemp(dir=static_dir.parent, prefix=".home-manager-static.")
    )
    try:
        shutil.copytree(
            static_source,
            temporary,
            dirs_exist_ok=True,
            symlinks=False,
        )
        make_user_writable(temporary)
        if lexists(static_dir):
            shutil.rmtree(static_dir)
        os.replace(temporary, static_dir)
    finally:
        if temporary.exists():
            shutil.rmtree(temporary)


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
        install_atomic(stamp_source, stamp)

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
