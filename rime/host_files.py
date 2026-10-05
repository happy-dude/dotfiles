import sys
from pathlib import Path

import dotfiles_files
from dotfiles_files import copy_file, fail, same_content


def validate_materialize(
    source: Path,
    target: Path,
    snapshot: Path,
    adopt: bool = False,
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
        if not adopt and not same_content(source, target):
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
    if target.is_symlink() or not target.exists():
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


def deploy(source_dir: Path) -> None:
    config_dir = dotfiles_files.config_home() / "fcitx5"
    state_root = dotfiles_files.state_home() / "rime/host-config"
    # A linked directory would carry managed writes to wherever it points.
    for directory in (config_dir, config_dir / "conf"):
        if directory.is_symlink():
            fail(f"Refusing to replace unmanaged Rime link: {directory}")
    # (source, target, snapshot, adopt). Fcitx writes notifications.conf
    # itself when a notification is hidden, so an existing copy is kept on the
    # first deploy instead of refused.
    files = (
        (
            source_dir / "profile",
            config_dir / "profile",
            state_root / "profile",
            False,
        ),
        (
            source_dir / "conf/classicui.conf",
            config_dir / "conf/classicui.conf",
            state_root / "classicui.conf",
            False,
        ),
        (
            source_dir / "conf/rime.conf",
            config_dir / "conf/rime.conf",
            state_root / "rime.conf",
            False,
        ),
        (
            source_dir / "conf/notifications.conf",
            config_dir / "conf/notifications.conf",
            state_root / "notifications.conf",
            True,
        ),
    )
    for source, target, snapshot, adopt in files:
        validate_materialize(source, target, snapshot, adopt)
    for source, target, snapshot, _ in files:
        materialize(source, target, snapshot)


def main(arguments: list[str]) -> None:
    if len(arguments) == 2 and arguments[0] == "deploy":
        deploy(Path(arguments[1]))
        return
    raise SystemExit("usage: rime-host-files deploy FCITX_CONFIG_DIR")


if __name__ == "__main__":
    main(sys.argv[1:])
