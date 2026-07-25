import re
import sys
from pathlib import Path

from dotfiles_files import PRESERVE, write_text

HEADER = re.compile(r'^\[submodule\s+"([^"]+)"\]$')


def sort_one(path: Path) -> None:
    if not path.exists():
        return
    if path.is_symlink() or not path.is_file():
        raise SystemExit(f"Refusing non-regular .gitmodules path: {path}")

    preamble: list[str] = []
    blocks: list[tuple[str, list[str]]] = []
    current_key: str | None = None
    current_lines: list[str] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        match = HEADER.fullmatch(line)
        if match:
            if current_key is not None:
                blocks.append((current_key, current_lines))
            current_key = match.group(1).removeprefix("vendor/")
            current_lines = [line]
        elif current_key is None:
            preamble.append(line)
        else:
            current_lines.append(line)
    if current_key is not None:
        blocks.append((current_key, current_lines))

    blocks.sort(key=lambda block: block[0].casefold())
    output_lines = preamble.copy()
    for _, lines in blocks:
        output_lines.append(lines[0])
        output_lines.extend("\t" + line.lstrip() for line in lines[1:])
    output = "\n".join(output_lines) + "\n"
    current = path.read_text(encoding="utf-8")
    if output == current:
        return

    write_text(path, output, PRESERVE)


def main(arguments: list[str]) -> None:
    paths = [Path(argument) for argument in arguments]
    for path in paths or [Path(".gitmodules")]:
        sort_one(path)


if __name__ == "__main__":
    main(sys.argv[1:])
