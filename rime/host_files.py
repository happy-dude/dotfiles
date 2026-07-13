import os
import shutil
import sys
import tempfile
from pathlib import Path


def fail(message: str) -> None:
    print(message, file=sys.stderr)
    raise SystemExit(1)


def exists(path: Path) -> bool:
    return os.path.lexists(path)


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


def validate_release_file(
    source: Path,
    target: Path,
    snapshot: Path,
) -> None:
    if target.is_symlink():
        if target.resolve(strict=False) != source.resolve(strict=False):
            fail(f"Refusing to release unmanaged Rime link: {target}")
    elif target.exists():
        if (
            not target.is_file()
            or not snapshot.is_file()
            or snapshot.is_symlink()
        ):
            fail(f"Refusing to release unmanaged Rime path: {target}")
        if not same_file_content(
            target,
            snapshot,
        ) and not same_file_content(target, source):
            fail(
                "Refusing to discard runtime-modified Rime host file: "
                f"{target}"
            )


def validate_link(
    source: Path,
    target: Path,
    legacy_source: Path | None = None,
) -> None:
    if not source.exists():
        fail(f"Rime source does not exist: {source}")
    if target.is_symlink():
        actual = target.resolve(strict=False)
        if actual == source.resolve(strict=False):
            return
        if legacy_source is not None and actual == legacy_source.resolve(
            strict=False
        ):
            return
        fail(f"Refusing to replace unmanaged Rime link: {target}")
    if target.exists():
        fail(f"Refusing to replace unmanaged Rime path: {target}")


def ensure_link(source: Path, target: Path) -> None:
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.is_symlink():
        if target.resolve(strict=False) == source.resolve(strict=False):
            return
        target.unlink()
    target.symlink_to(source)


def validate_release_link(source: Path, target: Path) -> None:
    if target.is_symlink():
        if target.resolve(strict=False) != source.resolve(strict=False):
            fail(f"Refusing to release unmanaged Rime link: {target}")
    elif target.exists():
        fail(f"Refusing to release unmanaged Rime path: {target}")


def main(arguments: list[str]) -> None:
    if len(arguments) != 3:
        raise SystemExit(
            "usage: rime-host-files deploy|release SOURCE_ROOT THEME_SOURCE"
        )

    operation = arguments[0]
    source_root = Path(arguments[1])
    theme_source = Path(arguments[2])
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
    theme_target = home / ".local/share/fcitx5/themes"

    if operation == "deploy":
        for source, target, snapshot in files:
            validate_materialize(source, target, snapshot)
        validate_link(
            theme_source,
            theme_target,
            source_root / ".local/share/fcitx5/themes",
        )
        for source, target, snapshot in files:
            materialize(source, target, snapshot)
        ensure_link(theme_source, theme_target)
    elif operation == "release":
        for source, target, snapshot in files:
            validate_release_file(source, target, snapshot)
        validate_release_link(theme_source, theme_target)
        for _, target, snapshot in files:
            if exists(target):
                target.unlink()
            snapshot.unlink(missing_ok=True)
        if exists(theme_target):
            theme_target.unlink()
    else:
        raise SystemExit(
            "usage: rime-host-files deploy|release SOURCE_ROOT THEME_SOURCE"
        )


if __name__ == "__main__":
    main(sys.argv[1:])
