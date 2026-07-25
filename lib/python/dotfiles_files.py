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
from typing import BinaryIO

PRESERVE = -1
"""Keep the destination's current permissions, for a file that exists."""


def fail(message: str, status: int = 1) -> None:
    print(message, file=sys.stderr)
    raise SystemExit(status)


def state_home() -> Path:
    value = os.environ.get("XDG_STATE_HOME")
    if value:
        return Path(value)
    return Path.home() / ".local" / "state"


def same_content(left: Path, right: Path) -> bool:
    """Compare two files without holding either wholly in memory.

    Rime ships dictionaries of several megabytes, and this runs for every
    managed file on every activation. A missing file counts as different, so
    that callers guarding a replacement fail closed.
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


def replace_atomically(
    target: Path,
    write: Callable[[BinaryIO], None],
    mode: int,
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
        os.replace(temporary, target)
        directory = os.open(target.parent, os.O_RDONLY | os.O_DIRECTORY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    finally:
        temporary.unlink(missing_ok=True)


def write_bytes(target: Path, data: bytes, mode: int) -> None:
    replace_atomically(target, lambda output: output.write(data), mode)


def write_text(target: Path, text: str, mode: int) -> None:
    write_bytes(target, text.encode("utf-8"), mode)


def copy_file(source: Path, target: Path, mode: int) -> None:
    def copy(output: BinaryIO) -> None:
        with source.open("rb") as input_file:
            shutil.copyfileobj(input_file, output)

    replace_atomically(target, copy, mode)
