import sys
from pathlib import Path

import dotfiles_files
from dotfiles_files import copy_file, fail, same_content


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
        if not same_content(source, target):
            fail(f"Refusing unmanaged Rime host file: {target}")
    elif target.is_file() and snapshot.is_file():
        source_changed = not same_content(source, snapshot)
        target_changed = not same_content(target, snapshot)
        target_matches_source = same_content(target, source)
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
        copy_file(source, target, 0o644)
        copy_file(source, snapshot, 0o600)
    elif not snapshot.exists():
        copy_file(source, snapshot, 0o600)
        target.chmod(0o644)
    elif same_content(source, snapshot):
        target.chmod(0o644)
    elif same_content(target, snapshot):
        copy_file(source, target, 0o644)
        copy_file(source, snapshot, 0o600)
    elif same_content(target, source):
        copy_file(source, snapshot, 0o600)
        target.chmod(0o644)


def migrate_theme_root(marker_source: Path) -> None:
    if not marker_source.is_file() or marker_source.is_symlink():
        fail(f"Rime ownership source is not a regular file: {marker_source}")

    marker = dotfiles_files.state_home() / "rime/home-manager-ownership-v1"
    target = dotfiles_files.data_home() / "fcitx5/themes"

    if marker.is_symlink() or (marker.exists() and not marker.is_file()):
        fail(f"Refusing malformed Rime ownership marker: {marker}")
    if marker.is_file() and not same_content(marker_source, marker):
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
    state_root = dotfiles_files.state_home() / "rime/host-config"
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
