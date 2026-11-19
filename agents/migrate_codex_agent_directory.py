"""Retire only outgoing HM directory links; never edit their referents."""

import argparse
import sys
from pathlib import Path

from dotfiles_files import fail, outgoing_entry


def prepare(store: Path, generation: Path, phase: str) -> None:
    home = Path.home()
    directories = [home / name for name in (".claude", ".codex", ".omp")]
    for directory in directories:
        if directory.is_symlink() or (
            directory.exists() and not directory.is_dir()
        ):
            fail(
                f"Refusing malformed state directory: {directory}. Back it "
                "up privately and restore it as a real directory; retry."
            )

    links = []
    for relative in (Path(".claude/agents"), Path(".codex/agents")):
        target = home / relative
        if not target.is_symlink():
            if target.exists() and not target.is_dir():
                fail(f"Move non-directory {target} aside and retry activation")
            continue
        entry = outgoing_entry(target, relative, generation, store)
        recovery = (
            f"Inspect {target}, move the link itself (not its contents) to "
            "an unused backup name and retry; restore any personal agents "
            "beside the new managed files."
        )
        if entry is None or not entry.is_symlink():
            fail(f"Unverified agent directory link. {recovery}")
        if phase == "check":
            # HM checks the incoming children through the still-live link.
            # A real collision must not require a destructive preflight.
            print(
                f"If HM reports a collision under {target}: {recovery}",
                file=sys.stderr,
            )
        links.append(target)

    if phase == "secure":
        for directory in directories:
            directory.mkdir(mode=0o700, exist_ok=True)
            directory.chmod(0o700)
    elif phase == "retire":
        for link in links:
            link.unlink()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("phase", choices=("check", "secure", "retire"))
    parser.add_argument("store", type=Path)
    parser.add_argument("generation", type=Path)
    args = parser.parse_args()
    prepare(args.store, args.generation, args.phase)
