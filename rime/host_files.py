import re
import shlex
import sys
from pathlib import Path
from typing import NamedTuple

import dotfiles_files
from dotfiles_files import copy_file, fail, same_content, write_text

HIDDEN_NOTIFICATIONS = "HiddenNotifications"
_SECTION = re.compile(r"^\s*\[(.*)\]\s*$")
_LIST_ENTRY = re.compile(r"^\s*\d+\s*=(.*)$")


def validate_materialize(source: Path, target: Path, snapshot: Path) -> None:
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
            # The two commands rely on materialize(): a snapshot equal to the
            # source leaves the target alone, and a missing target is written
            # from the source.
            fail(
                "Rime host file changed both declaratively and at runtime: "
                f"{target}\n"
                f"The declaration deployed last is recorded in {snapshot}.\n"
                "To keep the runtime file, record the new declaration as "
                "deployed:\n"
                f"  cp {shlex.quote(str(source))} "
                f"{shlex.quote(str(snapshot))}\n"
                "To take the new declaration, remove the runtime file:\n"
                f"  rm {shlex.quote(str(target))}\n"
                "Then activate again."
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


def _hidden_section(lines: list[str]) -> tuple[int, int] | None:
    """Locate the [HiddenNotifications] body as a half-open line range."""
    start = None
    for index, line in enumerate(lines):
        header = _SECTION.match(line)
        if header is None:
            continue
        if start is not None:
            return start, index
        if header.group(1).strip() == HIDDEN_NOTIFICATIONS:
            start = index + 1
    return None if start is None else (start, len(lines))


def hidden_notifications(text: str) -> list[str]:
    """List the notification IDs a notifications.conf hides, in file order."""
    lines = text.splitlines()
    section = _hidden_section(lines)
    if section is None:
        return []
    entries = (_LIST_ENTRY.match(line) for line in lines[slice(*section)])
    return [entry.group(1).strip() for entry in entries if entry is not None]


def merge_hidden_notifications(declared: list[str], runtime: str) -> str:
    """Return runtime with every declared notification ID hidden.

    Fcitx stores the list as numbered keys under [HiddenNotifications] and
    rewrites the file whenever the user hides a notification, so the runtime
    file is merged, not replaced: its entries keep their order, each declared
    ID that is missing is appended, and the keys are renumbered from 0. Every
    other section and key passes through, and a file that already hides the
    declared IDs comes back byte for byte.

    An ID removed from the declaration is not removed from the file: nothing
    tells it apart from one the user hid in Fcitx.
    """
    lines = runtime.splitlines()
    present = hidden_notifications(runtime)
    missing = [
        value for value in dict.fromkeys(declared) if value not in present
    ]
    section = _hidden_section(lines)
    if section is None:
        if not missing:
            return runtime
        # A section opens at the end of the file, after any top-level keys.
        separator = [""] if lines and lines[-1].strip() else []
        lines += [*separator, f"[{HIDDEN_NOTIFICATIONS}]"]
        section = (len(lines), len(lines))

    start, end = section
    body: list[str] = []
    after_entries = 0
    count = 0
    for line in lines[start:end]:
        entry = _LIST_ENTRY.match(line)
        if entry is None:
            body.append(line)
            continue
        body.append(f"{count}={entry.group(1)}")
        count += 1
        after_entries = len(body)
    body[after_entries:after_entries] = [
        f"{count + offset}={value}" for offset, value in enumerate(missing)
    ]
    if body == lines[start:end]:
        return runtime
    return "\n".join(lines[:start] + body + lines[end:]) + "\n"


def _read_text(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        fail(f"Refusing Rime host file that is not UTF-8: {path}")


def plan_hidden_notifications(source: Path, target: Path) -> str | None:
    """Return the text target should hold, or None when it is current."""
    if not source.is_file() or source.is_symlink():
        fail(f"Rime source is not a regular file: {source}")
    declared = _read_text(source)
    if target.is_symlink():
        if target.resolve(strict=False) != source.resolve(strict=False):
            fail(f"Refusing to replace unmanaged Rime link: {target}")
        return declared
    if not target.exists():
        return declared
    if not target.is_file():
        fail(f"Refusing to replace unmanaged Rime path: {target}")
    runtime = _read_text(target)
    merged = merge_hidden_notifications(
        hidden_notifications(declared), runtime
    )
    return None if merged == runtime else merged


class Plan(NamedTuple):
    """What deploy() writes once every refusal has been ruled out."""

    files: tuple[tuple[Path, Path, Path], ...]
    """(source, target, snapshot), replaced behind the snapshot guard."""
    notifications: Path
    merged: str | None
    """The text notifications should receive, or None when it is current."""


def validate(source_dir: Path) -> Plan:
    """Raise every refusal deploy() can raise, without writing anything.

    Activation runs this before Home Manager's write boundary, so a refusal
    stops it while the previous generation is still intact.
    """
    config_dir = dotfiles_files.config_home() / "fcitx5"
    state_root = dotfiles_files.state_home() / "rime/host-config"
    # A linked directory would carry managed writes to wherever it points.
    for directory in (
        config_dir,
        config_dir / "conf",
        state_root.parent,
        state_root,
    ):
        if directory.is_symlink():
            fail(f"Refusing to replace unmanaged Rime link: {directory}")
        if directory.exists() and not directory.is_dir():
            fail(f"Refusing non-directory Rime path: {directory}")
    files = (
        (
            source_dir / "profile",
            config_dir / "profile",
            state_root / "profile",
        ),
        (
            source_dir / "conf/classicui.conf",
            config_dir / "conf/classicui.conf",
            state_root / "classicui.conf",
        ),
        (
            source_dir / "conf/rime.conf",
            config_dir / "conf/rime.conf",
            state_root / "rime.conf",
        ),
    )
    # Fcitx owns notifications.conf and rewrites it at runtime, so it is
    # merged instead and has no snapshot.
    notifications = config_dir / "conf/notifications.conf"
    notifications_source = source_dir / "conf/notifications.conf"
    # rime/default.nix ships the whole tracked Fcitx tree as source_dir, and
    # only the files named here reach the home directory.
    declared = {source for source, _, _ in files} | {notifications_source}
    for path in sorted(source_dir.rglob("*")):
        if not path.is_dir() and path not in declared:
            fail(f"Rime source file is not materialized: {path}")
    for source, target, snapshot in files:
        validate_materialize(source, target, snapshot)
    merged = plan_hidden_notifications(notifications_source, notifications)
    return Plan(files, notifications, merged)


def deploy(source_dir: Path) -> None:
    plan = validate(source_dir)
    for source, target, snapshot in plan.files:
        materialize(source, target, snapshot)
    if plan.merged is not None:
        write_text(plan.notifications, plan.merged, 0o644)
    # notifications.conf has no snapshot. Remove one left in the state
    # directory, which nothing reads.
    obsolete = dotfiles_files.state_home() / (
        "rime/host-config/notifications.conf"
    )
    if obsolete.is_symlink() or obsolete.is_file():
        obsolete.unlink()


def main(arguments: list[str]) -> None:
    if len(arguments) == 2 and arguments[0] == "deploy":
        deploy(Path(arguments[1]))
        return
    if len(arguments) == 2 and arguments[0] == "validate":
        validate(Path(arguments[1]))
        return
    raise SystemExit(
        "usage: rime-host-files (deploy | validate) FCITX_CONFIG_DIR"
    )


if __name__ == "__main__":
    main(sys.argv[1:])
