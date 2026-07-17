#!/usr/bin/env python3

import argparse
import re
import subprocess
from pathlib import Path

SUBJECT_LIMIT = 72
BODY_LIMIT = 80
SUBJECT_PATTERN = re.compile(r"^[^:]+: .+")
TRAILER_PATTERN = re.compile(r"^[A-Za-z][A-Za-z0-9-]*: .+")


def lint(message_path: Path) -> list[str]:
    text = message_path.read_text(encoding="utf-8")
    lines = text.splitlines()
    errors: list[str] = []

    if not text.endswith("\n"):
        errors.append("commit message must end with a newline")
    if not lines or not lines[0]:
        errors.append("commit subject must not be empty")
        return errors
    if len(lines[0]) > SUBJECT_LIMIT:
        errors.append(f"commit subject exceeds {SUBJECT_LIMIT} characters")
    if not SUBJECT_PATTERN.fullmatch(lines[0]):
        errors.append("commit subject must use 'subsystem: summary' form")
    if len(lines) > 1 and lines[1]:
        errors.append("commit subject must be followed by a blank line")

    for line_number, line in enumerate(lines[1:], start=2):
        if line.rstrip() != line:
            errors.append(f"line {line_number} has trailing whitespace")
        if len(line) > BODY_LIMIT and not TRAILER_PATTERN.fullmatch(line):
            errors.append(
                f"line {line_number} exceeds {BODY_LIMIT} characters"
            )

    prettier = subprocess.run(
        ["prettier", "--check", "--parser", "markdown", str(message_path)],
        check=False,
        capture_output=True,
        text=True,
    )
    if prettier.returncode:
        errors.append(
            "commit message body does not pass Prettier Markdown lint"
        )

    return errors


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("message", type=Path)
    args = parser.parse_args()

    errors = lint(args.message)
    for error in errors:
        print(f"{args.message}: {error}")
    return int(bool(errors))


if __name__ == "__main__":
    raise SystemExit(main())
