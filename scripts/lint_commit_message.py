#!/usr/bin/env python3

import argparse
import difflib
import re
import subprocess
from pathlib import Path

SUBJECT_LIMIT = 72
BODY_LIMIT = 80
SUBJECT_PATTERN = re.compile(r"^[^:]+: .+")
GENERATED_SUBJECT_PATTERNS = (
    re.compile(r"^Merge .+"),
    re.compile(r'^(?:Revert|Reapply) ".+"$'),
    re.compile(r"^(?:fixup!|squash!|amend!) .+"),
    re.compile(r"^Squashed commit of the following:$"),
)
TRAILER_PATTERN = re.compile(r"^[A-Za-z][A-Za-z0-9-]*: .+")


def is_generated_subject(subject: str) -> bool:
    return any(
        pattern.fullmatch(subject) for pattern in GENERATED_SUBJECT_PATTERNS
    )


def lint(message_path: Path) -> list[str]:
    text = message_path.read_text(encoding="utf-8")
    lines = text.splitlines()
    errors: list[str] = []

    if not text.endswith("\n"):
        errors.append("commit message must end with a newline")
    if not lines or not lines[0]:
        errors.append("commit subject must not be empty")
        return errors
    if not is_generated_subject(lines[0]):
        if len(lines[0]) > SUBJECT_LIMIT:
            errors.append(f"commit subject exceeds {SUBJECT_LIMIT} characters")
        if not SUBJECT_PATTERN.fullmatch(lines[0]):
            errors.append("commit subject must use 'subsystem: summary' form")
    if len(lines) > 1 and lines[1]:
        errors.append("commit subject must be followed by a blank line")

    fence_len = 0
    for line_number, line in enumerate(lines[1:], start=2):
        if line.rstrip() != line:
            errors.append(f"line {line_number} has trailing whitespace")
        content = line.lstrip(" \t")
        indent = len(line.expandtabs(4)) - len(content)
        ticks = len(content) - len(content.lstrip("`"))
        if fence_len:
            if indent <= 3 and ticks >= fence_len and not content[ticks:]:
                fence_len = 0
        elif indent <= 3 and ticks >= 3 and "`" not in content[ticks:]:
            # CommonMark: an opening fence is indented at most three spaces,
            # its info string cannot contain a backtick (so an inline span
            # cannot masquerade as a fence), and its closer is nothing but at
            # least as many backticks. Prettier canonicalizes tilde fences to
            # backticks, so only backtick fences survive to the width check.
            fence_len = ticks
        if len(line) <= BODY_LIMIT:
            continue
        # Fenced or indented code, trailers, and lines with no breakable
        # whitespace (URLs, paths, hashes) cannot be rewrapped without damage.
        if (
            fence_len
            or TRAILER_PATTERN.fullmatch(line)
            or line.startswith(("    ", "\t"))
        ):
            continue
        if len(line.split()) == 1:
            continue
        errors.append(f"line {line_number} exceeds {BODY_LIMIT} characters")
    if fence_len:
        errors.append("fenced code block is never closed")

    try:
        prettier = subprocess.run(
            [
                "prettier",
                "--parser",
                "markdown",
                "--stdin-filepath",
                "COMMIT_EDITMSG.md",
            ],
            check=False,
            capture_output=True,
            input=text,
            text=True,
        )
    except OSError:
        errors.append("prettier is not on PATH")
        return errors
    if prettier.returncode:
        detail = prettier.stderr.strip() or (
            f"Prettier exited with status {prettier.returncode}"
        )
        errors.append(
            f"Prettier could not format the commit message: {detail}"
        )
    elif prettier.stdout != text:
        difference = "".join(
            difflib.unified_diff(
                text.splitlines(keepends=True),
                prettier.stdout.splitlines(keepends=True),
                fromfile="submitted message",
                tofile="Prettier output",
            )
        ).rstrip()
        errors.append(
            "commit message differs from Prettier output; edit it as shown:\n"
            f"{difference}"
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
