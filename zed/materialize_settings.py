import json
import stat
import sys
from pathlib import Path
from typing import NoReturn, TypeAlias, cast

import json5

from dotfiles_files import write_text

JsonValue: TypeAlias = (
    bool
    | int
    | float
    | str
    | list["JsonValue"]
    | dict[str, "JsonValue"]
    | None
)
SETTINGS_MODE = 0o600


def fail(message: str, error: Exception | None = None) -> NoReturn:
    if error is not None:
        message = f"{message}: {error}"
    print(message, file=sys.stderr)
    raise SystemExit(1)


def read_text(path: Path, description: str) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except OSError as error:
        fail(f"Unable to read {description} {path}", error)


def parse_json(
    text: str, path: Path, description: str, *, json5_enabled: bool
) -> JsonValue:
    try:
        value = json5.loads(text) if json5_enabled else json.loads(text)
        return cast("JsonValue", value)
    except ValueError as error:
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

    static_description = "managed Zed settings"
    static = parse_json(
        read_text(static_path, static_description),
        static_path,
        static_description,
        json5_enabled=False,
    )
    description = "existing Zed settings"
    current = read_text(target, description) if target.exists() else None
    dynamic = (
        parse_json(current, target, description, json5_enabled=True)
        if current is not None
        else {}
    )
    if not isinstance(dynamic, dict):
        fail(f"Refusing Zed settings that are not a JSON object: {target}")
    merged = merge(dynamic, static)
    rendered = json.dumps(merged, ensure_ascii=False, indent=2) + "\n"
    # Activation runs this on every switch.
    if (
        rendered == current
        and stat.S_IMODE(target.stat().st_mode) == SETTINGS_MODE
    ):
        return
    write_text(target, rendered, SETTINGS_MODE)


def main(arguments: list[str]) -> None:
    if len(arguments) != 2:
        raise SystemExit(
            "usage: materialize-zed-settings STATIC_SETTINGS TARGET"
        )
    materialize(Path(arguments[0]), Path(arguments[1]))


if __name__ == "__main__":
    main(sys.argv[1:])
