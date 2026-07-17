#!/usr/bin/env python3

import tempfile
from pathlib import Path

from lint_commit_message import lint


def write_message(directory: Path, name: str, message: str) -> Path:
    path = directory / name
    path.write_text(message, encoding="utf-8")
    return path


def main() -> None:
    with tempfile.TemporaryDirectory() as temporary_directory:
        directory = Path(temporary_directory)
        valid = write_message(
            directory,
            "valid.md",
            """checks: validate commit messages

Keep commit messages readable as Markdown and enforce the repository's subject
and body width limits before creating a commit.

Assisted-by: ChatGPT (gpt-5.6-sol, medium, OpenCode)
""",
        )
        assert not lint(valid)

        invalid_subject = write_message(
            directory,
            "invalid-subject.md",
            """This subject has no subsystem prefix

Explain the change.
""",
        )
        assert "commit subject must use 'subsystem: summary' form" in lint(
            invalid_subject
        )

        overlong_line = (
            "This ordinary prose line intentionally exceeds the repository's "
            "maximum commit message body width by several characters."
        )
        invalid_width = write_message(
            directory,
            "invalid-width.md",
            f"checks: reject long commit message lines\n\n{overlong_line}\n",
        )
        assert "line 3 exceeds 80 characters" in lint(invalid_width)


if __name__ == "__main__":
    main()
