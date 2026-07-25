import sys
from pathlib import Path

import tomlkit
from tomlkit.items import AoT, Table

from dotfiles_files import write_text

CONFIG_SCHEMA_URL = "https://developers.openai.com/codex/config-schema.json"
MANAGED_KEYS = (
    "developer_instructions",
    "model_reasoning_effort",
)
SCHEMA_DIRECTIVE = f"#:schema {CONFIG_SCHEMA_URL}"


def load_document(path: Path, description: str):
    try:
        return tomlkit.parse(path.read_text(encoding="utf-8"))
    except (OSError, tomlkit.exceptions.ParseError) as error:
        message = f"Unable to read {description} {path}: {error}"
        raise SystemExit(message) from error


def materialize(source: Path, target: Path) -> None:
    if not source.is_file():
        message = f"Generated Codex profile is not a regular file: {source}"
        raise SystemExit(message)

    generated = load_document(source, "generated Codex profile")
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

    runtime = (
        load_document(target, "existing Codex profile")
        if target.exists()
        else tomlkit.document()
    )
    merged = tomlkit.document()
    for key in MANAGED_KEYS:
        if key in generated:
            merged.add(key, generated.item(key))
    runtime_items = [
        (key, item, item.unwrap())
        for key, item in runtime.items()
        if key not in MANAGED_KEYS
    ]
    for key, item, value in runtime_items:
        if not isinstance(item, (Table, AoT)):
            merged[key] = value
    for key, item, value in runtime_items:
        if isinstance(item, (Table, AoT)):
            merged[key] = value

    write_text(target, SCHEMA_DIRECTIVE + "\n" + tomlkit.dumps(merged), 0o600)


def main(arguments: list[str]) -> None:
    if len(arguments) != 2:
        raise SystemExit("usage: materialize-codex-profile SOURCE TARGET")
    materialize(Path(arguments[0]), Path(arguments[1]))


if __name__ == "__main__":
    main(sys.argv[1:])
