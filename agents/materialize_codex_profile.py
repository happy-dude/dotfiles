import stat
import sys
from pathlib import Path

import tomlkit

from dotfiles_files import write_text

# Keep in sync with profileFiles in agents/default.nix: unexpected template
# keys fail activation, and keys listed here but absent from a template are
# removed from the target profile.
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


def materialize(source: Path, target: Path, *, check_only: bool) -> None:
    """Merge the generated keys into target, or refuse and leave it alone.

    With check_only, make every refusal a real run would make and write
    nothing. Activation calls this before Home Manager switches any link, so
    a profile it cannot merge stops the switch while the previous generation
    is still in place.
    """
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
    if target.parent.is_symlink() or (
        target.parent.exists() and not target.parent.is_dir()
    ):
        raise SystemExit(
            f"Refusing malformed Codex profile directory: {target.parent}"
        )

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
    if check_only:
        return
    # Activation runs this on every switch.
    if (
        rendered == runtime_text
        and stat.S_IMODE(target.stat().st_mode) == PROFILE_MODE
    ):
        return
    write_text(target, rendered, PROFILE_MODE)


def main(arguments: list[str]) -> None:
    check_only = arguments[:1] == ["--check"]
    paths = arguments[1:] if check_only else arguments
    if len(paths) != 2:
        raise SystemExit(
            "usage: materialize-codex-profile [--check] SOURCE TARGET"
        )
    materialize(Path(paths[0]), Path(paths[1]), check_only=check_only)


if __name__ == "__main__":
    main(sys.argv[1:])
