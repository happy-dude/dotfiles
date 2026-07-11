#!/usr/bin/env python3
"""Generate Codex agents and profiles from canonical Claude Markdown prompts."""

from __future__ import annotations

import argparse
import json
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
SOURCE_DIR = ROOT / "agents" / "prompts"
AGENT_DIR = ROOT / "agents" / "generated" / "codex-agents"
PROFILE_DIR = ROOT / "agents" / "generated" / "codex-profiles"

AGENTS = ("kernel", "language")


def prompt(name: str) -> tuple[dict[str, str], str]:
    text = (SOURCE_DIR / f"{name}.md").read_text()
    if not text.startswith("---\n"):
        raise ValueError(f"missing frontmatter: {name}.md")
    frontmatter, separator, text = text[4:].partition("\n---\n")
    if not separator:
        raise ValueError(f"unterminated frontmatter: {name}.md")
    metadata = {}
    for line in frontmatter.splitlines():
        key, separator, value = line.partition(":")
        if separator and key in {"name", "description"}:
            metadata[key] = value.strip()
    for key in ("name", "description"):
        if not metadata.get(key):
            raise ValueError(f"missing {key} in frontmatter: {name}.md")
    if metadata["name"] != name:
        raise ValueError(f"frontmatter name does not match filename: {name}.md")
    text = text.strip() + "\n"
    if "'''" in text:
        raise ValueError(f"prompt cannot contain TOML literal delimiter: {name}.md")
    return metadata, text


def agent_toml(metadata: dict[str, str], body: str) -> str:
    return (
        f"name = {json.dumps(metadata['name'])}\n"
        f"description = {json.dumps(metadata['description'])}\n"
        "developer_instructions = '''\n"
        f"{body}'''\n"
    )


def profile_toml(name: str, body: str) -> str:
    suffix = 'model_reasoning_effort = "medium"\n' if name == "kernel" else ""
    return f"developer_instructions = '''\n{body}'''\n{suffix}"


def outputs() -> dict[pathlib.Path, str]:
    result = {}
    for name in AGENTS:
        metadata, body = prompt(name)
        result[AGENT_DIR / f"{name}.toml"] = agent_toml(metadata, body)
        result[PROFILE_DIR / f"{name}.config.toml"] = profile_toml(name, body)
    return result


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    stale = []
    for path, content in outputs().items():
        if args.check:
            if not path.exists() or path.read_text() != content:
                stale.append(path.relative_to(ROOT))
        else:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(content)
    if stale:
        print("stale generated Codex agent files:", file=sys.stderr)
        for path in stale:
            print(f"  {path}", file=sys.stderr)
        print("run scripts/generate_codex_agents.py", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
