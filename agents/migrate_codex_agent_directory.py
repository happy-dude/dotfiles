import os
import sys
from pathlib import Path


def fail(message: str, status: int = 1) -> None:
    print(message, file=sys.stderr)
    raise SystemExit(status)


def main(arguments: list[str]) -> None:
    if len(arguments) < 3:
        fail(
            "usage: migrate-codex-agent-directory "
            "LEGACY TARGET NAME=SOURCE...",
            2,
        )

    legacy = Path(arguments[0])
    target = Path(arguments[1])
    remove_target = False
    legacy_links: list[Path] = []

    if target.is_symlink():
        if target.resolve(strict=False) != legacy.resolve(strict=False):
            fail(f"Refusing unrelated Codex agent directory link: {target}")
        remove_target = True
    elif target.exists() and not target.is_dir():
        fail(f"Refusing non-directory Codex agent path: {target}")

    for mapping in arguments[2:]:
        if "=" not in mapping:
            fail(f"Invalid Codex agent migration mapping: {mapping}", 2)
        name, source_text = mapping.split("=", 1)
        if not name or "/" in name:
            fail(f"Invalid Codex agent migration mapping: {mapping}", 2)

        source = Path(source_text)
        legacy_path = legacy / f"{name}.toml"
        if legacy_path.is_symlink():
            if legacy_path.resolve(strict=False) != source.resolve(
                strict=False
            ):
                link_target = os.readlink(legacy_path)
                suffix = f"/.codex/agents/{name}.toml"
                if not link_target.endswith(suffix):
                    fail(
                        "Refusing unrelated legacy Codex agent link: "
                        f"{legacy_path}"
                    )
                home_files = Path(link_target.removesuffix(suffix))
                store_dir = source.parent
                if (
                    not remove_target
                    or home_files == Path(link_target)
                    or home_files.parent != store_dir
                    or not home_files.name.endswith("-home-manager-files")
                ):
                    fail(
                        "Refusing unrelated legacy Codex agent link: "
                        f"{legacy_path}"
                    )
            legacy_links.append(legacy_path)
        elif legacy_path.exists():
            fail(f"Refusing unmanaged legacy Codex agent file: {legacy_path}")

    for legacy_path in legacy_links:
        legacy_path.unlink()
    if remove_target:
        target.unlink()


if __name__ == "__main__":
    main(sys.argv[1:])
