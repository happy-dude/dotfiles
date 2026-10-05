import stat
import sys
from pathlib import Path

import tomlkit

from dotfiles_files import write_text

MANAGED_KEYS = (
    "developer_instructions",
    "model_reasoning_effort",
)
PROFILE_MODE = 0o600


def read_text(path: Path, description: str) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except OSError as error:
        message = f"Unable to read {description} {path}: {error}"
        raise SystemExit(message) from error


def parse_document(text: str, path: Path, description: str):
    try:
        return tomlkit.parse(text)
    except tomlkit.exceptions.ParseError as error:
        message = f"Unable to read {description} {path}: {error}"
        raise SystemExit(message) from error


def materialize(source: Path, target: Path) -> None:
    if not source.is_file():
        message = f"Generated Codex profile is not a regular file: {source}"
        raise SystemExit(message)

    description = "generated Codex profile"
    source_text = read_text(source, description)
    generated = parse_document(source_text, source, description)
    # Nix writes the schema directive into the template; carry it over
    # verbatim.
    first_line = source_text.split("\n", 1)[0]
    directive = first_line + "\n" if first_line.startswith("#:schema ") else ""
    unexpected = set(generated) - set(MANAGED_KEYS)
    if unexpected:
        names = ", ".join(sorted(unexpected))
        message = f"Generated Codex profile has unmanaged keys: {names}"
        raise SystemExit(message)
    if "developer_instructions" not in generated:
        raise SystemExit(
            "Generated Codex profile lacks developer_instructions"
        )

    if target.is_symlink():
        raise SystemExit(f"Refusing symlinked Codex profile: {target}")
    if target.exists() and not target.is_file():
        raise SystemExit(f"Refusing non-regular Codex profile: {target}")

    description = "existing Codex profile"
    runtime_text = read_text(target, description) if target.exists() else None
    runtime = (
        parse_document(runtime_text, target, description)
        if runtime_text is not None
        else tomlkit.document()
    )
    merged = tomlkit.document()
    for key in MANAGED_KEYS:
        if key in generated:
            merged.add(key, generated.item(key))
    # tomlkit places a scalar added after a table before the first table
    # header, so runtime keys can be copied in their original order.
    # Iterating returns top-level booleans as plain bool, which has no
    # unwrap(); runtime.item() always returns a tomlkit item.
    for key in runtime:
        if key not in MANAGED_KEYS:
            merged[key] = runtime.item(key).unwrap()

    rendered = directive + tomlkit.dumps(merged)
    # Activation runs this on every switch.
    if (
        rendered == runtime_text
        and stat.S_IMODE(target.stat().st_mode) == PROFILE_MODE
    ):
        return
    write_text(target, rendered, PROFILE_MODE)


def main(arguments: list[str]) -> None:
    if len(arguments) != 2:
        raise SystemExit("usage: materialize-codex-profile SOURCE TARGET")
    materialize(Path(arguments[0]), Path(arguments[1]))


if __name__ == "__main__":
    main(sys.argv[1:])
