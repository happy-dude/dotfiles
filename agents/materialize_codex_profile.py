import argparse
import stat
import sys
from pathlib import Path

import tomlkit

from dotfiles_files import guard_current_file, outgoing_store_file, write_bytes

# The keys profileFiles in agents/default.nix generates. A template key that
# is not listed here fails activation, and a listed key that a template
# leaves out is removed from that profile. The codex-profile-materializer
# check fails unless the templates carry exactly these keys between them. A
# key retired from both is no longer managed, so an existing profile keeps
# its last value.
MANAGED_KEYS = (
    "developer_instructions",
    "model_reasoning_effort",
)
PROFILE_MODE = 0o600


def read_text(path: Path, description: str) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except (OSError, UnicodeError) as error:
        message = f"Unable to read {description} {path}: {error}"
        raise SystemExit(message) from error


def parse_document(text: str, path: Path, description: str):
    try:
        return tomlkit.parse(text)
    except tomlkit.exceptions.ParseError as error:
        message = f"Unable to read {description} {path}: {error}"
        raise SystemExit(message) from error


def materialize(
    source: Path,
    target: Path,
    *,
    check_only: bool,
    old_generation: Path,
) -> None:
    """Merge the generated keys into target, or refuse and leave it alone.

    Preflight parses, merges and encodes without writing. A real run reads
    current bytes again; neither phase locks out a running Codex client.
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

    if target.parent.is_symlink() or (
        target.parent.exists() and not target.parent.is_dir()
    ):
        raise SystemExit(
            f"Refusing malformed Codex profile directory: {target.parent}"
        )

    unchanged = guard_current_file(target)
    runtime_path = target
    if target.is_symlink():
        # profileFiles are individual direct store outputs. Neither arriving
        # at the same bytes nor linking to any store path proves ownership.
        runtime_path = outgoing_store_file(
            target, Path(".codex") / target.name, old_generation, source.parent
        )
    elif target.exists() and not target.is_file():
        raise SystemExit(f"Refusing non-regular Codex profile: {target}")

    description = "existing Codex profile"
    runtime_text = (
        read_text(runtime_path, description) if runtime_path.exists() else None
    )
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
    data = rendered.encode("utf-8")
    if check_only:
        return
    # Activation runs this on every switch.
    if (
        runtime_path == target
        and rendered == runtime_text
        and stat.S_IMODE(target.stat().st_mode) == PROFILE_MODE
    ):
        return
    write_bytes(target, data, PROFILE_MODE, before_replace=unchanged)


def main(arguments: list[str]) -> None:
    parser = argparse.ArgumentParser(description="Merge a writable profile")
    parser.add_argument("--check", action="store_true")
    parser.add_argument("--old-generation", type=Path, default=Path())
    parser.add_argument("source", type=Path)
    parser.add_argument("target", type=Path)
    args = parser.parse_args(arguments)
    materialize(
        args.source,
        args.target,
        check_only=args.check,
        old_generation=args.old_generation,
    )


if __name__ == "__main__":
    main(sys.argv[1:])
