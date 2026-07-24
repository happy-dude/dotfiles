import os
import shutil
import sys
import tempfile
from pathlib import Path


def fail(message: str) -> None:
    print(message, file=sys.stderr)
    raise SystemExit(1)


def install_atomic(source: Path, target: Path, mode: int) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        dir=target.parent,
        prefix=".rime-host-file.",
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as output:
            with source.open("rb") as input_file:
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


def same_file_content(left: Path, right: Path) -> bool:
    if left.stat().st_size != right.stat().st_size:
        return False
    with left.open("rb") as left_file, right.open("rb") as right_file:
        while True:
            left_chunk = left_file.read(1024 * 1024)
            right_chunk = right_file.read(1024 * 1024)
            if left_chunk != right_chunk:
                return False
            if not left_chunk:
                return True


def validate_materialize(
    source: Path,
    target: Path,
    snapshot: Path,
) -> None:
    if not source.is_file() or source.is_symlink():
        fail(f"Rime source is not a regular file: {source}")
    if snapshot.is_symlink() or (snapshot.exists() and not snapshot.is_file()):
        fail(f"Refusing malformed Rime host snapshot: {snapshot}")
    if target.is_symlink():
        if target.resolve(strict=False) != source.resolve(strict=False):
            fail(f"Refusing to replace unmanaged Rime link: {target}")
    elif target.exists() and not target.is_file():
        fail(f"Refusing to replace unmanaged Rime path: {target}")
    elif target.is_file() and not snapshot.exists():
        if not same_file_content(source, target):
            fail(f"Refusing unmanaged Rime host file: {target}")
    elif target.is_file() and snapshot.is_file():
        source_changed = not same_file_content(source, snapshot)
        target_changed = not same_file_content(target, snapshot)
        target_matches_source = same_file_content(target, source)
        if source_changed and target_changed and not target_matches_source:
            fail(
                "Rime host file changed both declaratively and at runtime: "
                f"{target}"
            )


def materialize(source: Path, target: Path, snapshot: Path) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    snapshot.parent.mkdir(parents=True, exist_ok=True)
    if target.is_symlink():
        target.unlink()

    if not target.exists():
        install_atomic(source, target, 0o644)
        install_atomic(source, snapshot, 0o600)
    elif not snapshot.exists():
        install_atomic(source, snapshot, 0o600)
        target.chmod(0o644)
    elif same_file_content(source, snapshot):
        target.chmod(0o644)
    elif same_file_content(target, snapshot):
        install_atomic(source, target, 0o644)
        install_atomic(source, snapshot, 0o600)
    elif same_file_content(target, source):
        install_atomic(source, snapshot, 0o600)
        target.chmod(0o644)


def migrate_theme_root(marker_source: Path) -> None:
    if not marker_source.is_file() or marker_source.is_symlink():
        fail(f"Rime ownership source is not a regular file: {marker_source}")

    home = Path.home()
    state_home = Path(os.environ.get("XDG_STATE_HOME", home / ".local/state"))
    marker = state_home / "rime/home-manager-ownership-v1"
    target = home / ".local/share/fcitx5/themes"

    if marker.is_symlink() or (marker.exists() and not marker.is_file()):
        fail(f"Refusing malformed Rime ownership marker: {marker}")
    if marker.is_file() and not same_file_content(marker_source, marker):
        fail(f"Refusing unrecognized Rime ownership marker: {marker}")

    if target.is_symlink():
        actual = target.resolve(strict=False)
        managed_store_link = actual.parent == Path(
            "/nix/store"
        ) and actual.name.endswith("-fcitx5-themes")
        if not marker.is_file() or not managed_store_link:
            fail(f"Refusing to migrate unmanaged Rime link: {target}")
        target.unlink()
    elif target.exists() and not target.is_dir():
        fail(f"Refusing to migrate unmanaged Rime path: {target}")

    marker.unlink(missing_ok=True)


def deploy(source_root: Path) -> None:
    home = Path.home()
    state_home = Path(os.environ.get("XDG_STATE_HOME", home / ".local/state"))
    state_root = state_home / "rime/host-config"
    files = (
        (
            source_root / ".config/fcitx5/profile",
            home / ".config/fcitx5/profile",
            state_root / "profile",
        ),
        (
            source_root / ".config/fcitx5/conf/classicui.conf",
            home / ".config/fcitx5/conf/classicui.conf",
            state_root / "classicui.conf",
        ),
        (
            source_root / ".config/fcitx5/conf/rime.conf",
            home / ".config/fcitx5/conf/rime.conf",
            state_root / "rime.conf",
        ),
    )
    for source, target, snapshot in files:
        validate_materialize(source, target, snapshot)
    for source, target, snapshot in files:
        materialize(source, target, snapshot)


def main(arguments: list[str]) -> None:
    if len(arguments) == 2 and arguments[0] == "deploy":
        deploy(Path(arguments[1]))
        return
    if len(arguments) == 2 and arguments[0] == "migrate-theme-root":
        migrate_theme_root(Path(arguments[1]))
        return
    raise SystemExit(
        "usage: rime-host-files "
        "deploy SOURCE_ROOT | migrate-theme-root MARKER_SOURCE"
    )


if __name__ == "__main__":
    main(sys.argv[1:])
