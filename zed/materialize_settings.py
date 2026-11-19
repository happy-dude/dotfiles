import json
import stat
import sys
from pathlib import Path
from typing import NoReturn, TypeAlias, cast

import json5

from dotfiles_files import guard_current_file, write_bytes

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
    except (OSError, UnicodeError) as error:
        fail(f"Unable to read {description} {path}", error)


def parse_json(
    text: str, path: Path, description: str, *, json5_enabled: bool
) -> JsonValue:
    try:
        value = json5.loads(text) if json5_enabled else json.loads(text)
        if json5_enabled and "\\u" in text:
            # json5 can leave escaped non-BMP characters as two surrogate
            # code units. Join valid pairs in keys as well as values before
            # merging; a lone surrogate still fails strict UTF-8 encoding.
            value = json.loads(json.dumps(value))
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


def materialize(static_path: Path, target: Path, *, check_only: bool) -> None:
    """Merge the managed settings into target, or refuse and leave it alone.

    Check mode parses and encodes the same output without writing. A real
    run reloads the client file rather than publishing the preflight's copy.
    """
    unchanged = guard_current_file(target)
    if target.is_symlink():
        fail(f"Refusing symlinked Zed settings: {target}")
    if target.exists() and not target.is_file():
        fail(f"Refusing non-regular Zed settings: {target}")
    if target.parent.is_symlink() or (
        target.parent.exists() and not target.parent.is_dir()
    ):
        fail(f"Refusing malformed Zed settings directory: {target.parent}")

    static_description = "managed Zed settings"
    static = parse_json(
        read_text(static_path, static_description),
        static_path,
        static_description,
        json5_enabled=False,
    )
    if not isinstance(static, dict):
        fail(f"Managed Zed settings are not a JSON object: {static_path}")
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
    try:
        rendered = (
            json.dumps(merged, ensure_ascii=False, allow_nan=False, indent=2)
            + "\n"
        )
        data = rendered.encode("utf-8")
    except ValueError as error:
        fail(f"Cannot serialize Zed settings as UTF-8 JSON: {target}", error)
    if check_only:
        return
    # Activation runs this on every switch.
    if (
        rendered == current
        and stat.S_IMODE(target.stat().st_mode) == SETTINGS_MODE
    ):
        return
    write_bytes(target, data, SETTINGS_MODE, before_replace=unchanged)


def main(arguments: list[str]) -> None:
    check_only = arguments[:1] == ["--check"]
    paths = arguments[1:] if check_only else arguments
    if len(paths) != 2:
        raise SystemExit(
            "usage: materialize-zed-settings [--check] STATIC_SETTINGS TARGET"
        )
    materialize(Path(paths[0]), Path(paths[1]), check_only=check_only)


if __name__ == "__main__":
    main(sys.argv[1:])
