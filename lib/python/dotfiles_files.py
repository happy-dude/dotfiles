"""Durable file replacement shared by the activation helpers.

Activation runs while the user's session may be using the files being
written, and a crash between truncation and the final write would leave
configuration the owning program cannot parse. Every writer here stages
into a temporary file in the destination directory, fsyncs it, renames it
over the target, and then fsyncs the directory so the rename itself is
durable.
"""

import os
import shutil
import sys
import tempfile
from collections.abc import Callable
from pathlib import Path
from typing import BinaryIO, NoReturn

PRESERVE = -1
"""Keep the destination's current permissions, for a file that exists."""


def fail(message: str, status: int = 1) -> NoReturn:
    print(message, file=sys.stderr)
    raise SystemExit(status)


def _base_directory(variable: str, fallback: Path) -> Path:
    # The base-directory specification requires absolute paths and says a
    # relative value is invalid and must be ignored.
    value = os.environ.get(variable)
    if value and os.path.isabs(value):
        return Path(value)
    return fallback


def config_home() -> Path:
    return _base_directory("XDG_CONFIG_HOME", Path.home() / ".config")


def state_home() -> Path:
    return _base_directory("XDG_STATE_HOME", Path.home() / ".local" / "state")


def data_home() -> Path:
    return _base_directory("XDG_DATA_HOME", Path.home() / ".local" / "share")


def same_content(left: Path, right: Path) -> bool:
    """Compare two files without holding either wholly in memory.

    Rime's host-file guards compare the declaration, runtime file, and last
    deployed snapshot. A missing file is different, not an empty file.
    """
    try:
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
    except FileNotFoundError:
        return False


def _resolve_mode(target: Path, mode: int) -> int:
    if mode != PRESERVE:
        return mode
    try:
        return target.stat().st_mode & 0o777
    except FileNotFoundError:
        return 0o600


def home_relative_target(target: Path) -> Path | None:
    """Locate the home by identity, without resolving the target through it.

    HM may name /home/user where HOME names /var/home/user. Only the home
    prefix is trusted: take the first such ancestor, so a link *below* it
    pointing back into the home cannot hide a redirected component.
    """
    if not target.is_absolute() or ".." in target.parts:
        return None
    home = Path.home()
    for ancestor in reversed(target.parents):
        if ancestor.is_dir() and ancestor.samefile(home):
            relative = target.relative_to(ancestor)
            for parent in relative.parents:
                if parent.parts and (ancestor / parent).is_symlink():
                    fail(
                        f"Refusing redirected directory: {ancestor / parent}. "
                        "Back it up privately, restore a real directory in "
                        "its place and retry activation."
                    )
            return relative
    return None


def outgoing_entry(
    target: Path, relative: Path, generation: Path, store: Path
) -> Path | None:
    """Match the literal home link against HM's *outgoing* home-files tree.

    generation comes from activation's oldGenPath, never from the link we
    are testing. A matching final realpath, basename or store prefix alone
    proves nothing. The caller decides which manifest leaves it supports.
    """
    if (
        not generation.is_absolute()
        or generation.parent != store
        or generation.is_symlink()
    ):
        return None
    try:
        files = (generation / "home-files").readlink()
        if files.parent != store or files.is_symlink() or not files.is_dir():
            return None
        entry = files / relative
        if (
            home_relative_target(target) != relative
            or entry.parent.resolve(strict=True) != entry.parent
            or os.readlink(target) != str(entry)
        ):
            return None
        return entry
    except (OSError, RuntimeError):
        return None


def outgoing_store_file(
    target: Path, relative: Path, generation: Path, store: Path
) -> Path:
    """Only adopt regular, immutable data, never an out-of-store referent."""
    entry = outgoing_entry(target, relative, generation, store)
    if entry is not None:
        try:
            source = entry.readlink() if entry.is_symlink() else entry
            if (
                source.is_relative_to(store)
                and source.resolve(strict=True) == source
                and source.is_file()
            ):
                return source
        except (OSError, RuntimeError):
            pass
    fail(
        f"Refusing unverified link: {target}. Stop the client, inspect the "
        "link and back up its contents privately. Move the link itself to an "
        "unused backup name, restore any wanted contents here as a regular "
        "mode-0600 file, then retry activation."
    )


def guard_current_file(target: Path) -> Callable[[], None]:
    """Sample before a read/merge; check again after staging the result.

    This detects intervening saves, not one racing the final check/rename.
    Quiesce the client when losing a concurrent save is unacceptable.
    """

    def version() -> tuple[int, ...] | None:
        try:
            info = target.lstat()
        except FileNotFoundError:
            return None
        return (
            info.st_dev,
            info.st_ino,
            info.st_size,
            info.st_mtime_ns,
            info.st_ctime_ns,
        )

    initial = version()

    def check() -> None:
        if version() != initial:
            fail(f"{target} changed during activation; stop client and retry")

    return check


def replace_atomically(
    target: Path,
    write: Callable[[BinaryIO], None],
    mode: int,
    *,
    before_replace: Callable[[], None] | None = None,
) -> None:
    """Replace target with whatever write() emits, atomically and durably."""
    target.parent.mkdir(parents=True, exist_ok=True)
    resolved = _resolve_mode(target, mode)
    descriptor, temporary_name = tempfile.mkstemp(
        dir=target.parent,
        prefix=f".{target.name}.",
    )
    temporary = Path(temporary_name)
    try:
        with os.fdopen(descriptor, "wb") as output:
            write(output)
            os.fchmod(output.fileno(), resolved)
            output.flush()
            os.fsync(output.fileno())
        if before_replace is not None:
            before_replace()
        os.replace(temporary, target)
        directory = os.open(target.parent, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    finally:
        temporary.unlink(missing_ok=True)


def write_bytes(
    target: Path,
    data: bytes,
    mode: int,
    *,
    before_replace: Callable[[], None] | None = None,
) -> None:
    replace_atomically(
        target,
        lambda output: output.write(data),
        mode,
        before_replace=before_replace,
    )


def write_text(target: Path, text: str, mode: int) -> None:
    write_bytes(target, text.encode("utf-8"), mode)


def copy_file(source: Path, target: Path, mode: int) -> None:
    def copy(output: BinaryIO) -> None:
        with source.open("rb") as input_file:
            shutil.copyfileobj(input_file, output)

    replace_atomically(target, copy, mode)
