import json
import sys
from pathlib import Path
from typing import TypeAlias, cast

import json5

from dotfiles_files import write_text

JsonValue: TypeAlias = (
    None
    | bool
    | int
    | float
    | str
    | list["JsonValue"]
    | dict[str, "JsonValue"]
)


def fail(message: str, error: Exception | None = None) -> None:
    if error is not None:
        message = f"{message}: {error}"
    print(message, file=sys.stderr)
    raise SystemExit(1)


def load_json(
    path: Path, description: str, *, json5_enabled: bool
) -> JsonValue:
    try:
        text = path.read_text(encoding="utf-8")
        value = json5.loads(text) if json5_enabled else json.loads(text)
        return cast("JsonValue", value)
    except (OSError, ValueError) as error:
        fail(f"Unable to read {description} {path}", error)


def merge(dynamic: JsonValue, static: JsonValue) -> JsonValue:
    if isinstance(dynamic, dict) and isinstance(static, dict):
        result = dynamic.copy()
        for key, value in static.items():
            result[key] = merge(result.get(key), value)
        return result
    return static


def materialize(static_path: Path, target: Path) -> None:
    if target.is_symlink():
        fail(f"Refusing symlinked Zed settings: {target}")
    if target.exists() and not target.is_file():
        fail(f"Refusing non-regular Zed settings: {target}")

    static = load_json(
        static_path, "managed Zed settings", json5_enabled=False
    )
    dynamic = (
        load_json(target, "existing Zed settings", json5_enabled=True)
        if target.exists()
        else {}
    )
    merged = merge(dynamic, static)
    rendered = json.dumps(merged, ensure_ascii=False, indent=2)
    write_text(target, rendered + "\n", 0o600)


def main(arguments: list[str]) -> None:
    if len(arguments) != 2:
        raise SystemExit(
            "usage: materialize-zed-settings STATIC_SETTINGS TARGET"
        )
    materialize(Path(arguments[0]), Path(arguments[1]))


if __name__ == "__main__":
    main(sys.argv[1:])
