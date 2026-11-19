"""Detach the outgoing HM-owned cli.json, without replaying it on switches."""

import argparse
from pathlib import Path

from dotfiles_files import (
    fail,
    guard_current_file,
    home_relative_target,
    outgoing_store_file,
    write_bytes,
)


def adopt(target: Path, generation: Path, store: Path, *, check: bool) -> None:
    relative = home_relative_target(target)
    if target.parent.is_symlink() or (
        target.parent.exists() and not target.parent.is_dir()
    ):
        fail(f"Restore {target.parent} as a real directory and retry")
    unchanged = guard_current_file(target)
    if not target.is_symlink():
        if target.exists() and not target.is_file():
            fail(f"Move non-regular {target} aside and retry activation")
        return
    if relative is None:
        fail(
            f"Cannot adopt {target} outside the Home Manager home. Stop "
            "OpenCode, back up its contents privately, move the link to an "
            "unused name, and restore a regular 0600 file before retrying."
        )
    source = outgoing_store_file(target, relative, generation, store)
    if not check:
        write_bytes(
            target, source.read_bytes(), 0o600, before_replace=unchanged
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument("target", type=Path)
    parser.add_argument("generation", type=Path)
    parser.add_argument("store", type=Path)
    args = parser.parse_args()
    adopt(args.target, args.generation, args.store, check=args.check)
