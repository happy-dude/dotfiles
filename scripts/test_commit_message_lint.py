#!/usr/bin/env python3

import os
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

        git_directory = directory / ".git"
        git_directory.mkdir()
        commit_editmsg = write_message(
            git_directory,
            "COMMIT_EDITMSG",
            """git: lint commit metadata paths

Prettier must validate this message even though Git stores it under .git.

Assisted-by: ChatGPT (gpt-5, medium, Codex)
Signed-off-by: Stanley Chan <schan@lostsanctum.dev>
""",
        )
        assert not lint(commit_editmsg)

        generated_subjects = (
            "Merge branch 'main' into macos",
            'Revert "checks: validate commit messages"',
            'Reapply "checks: validate commit messages"',
            "fixup! checks: validate commit messages",
            "squash! checks: validate commit messages",
            "amend! checks: validate commit messages",
            "Squashed commit of the following:",
        )
        for index, subject in enumerate(generated_subjects):
            generated = write_message(
                directory,
                f"generated-{index}.md",
                f"{subject}\n",
            )
            assert not lint(generated), subject

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

        long_url = "https://example.invalid/" + "documentation" * 6
        unbreakable = write_message(
            directory,
            "unbreakable.md",
            "checks: allow unbreakable body lines\n\n"
            f"{long_url}\n"
            "\n"
            "Prose around it still wraps normally.\n",
        )
        assert not lint(unbreakable)

        fenced_code = write_message(
            directory,
            "fenced-code.md",
            "checks: allow long fenced code lines\n\n```\n"
            f"{'long_code_line' * 8}\n"
            "```\n",
        )
        assert not lint(fenced_code)

        long_fence = write_message(
            directory,
            "long-fence.md",
            "checks: reject Prettier-rewritten four-backtick fences\n\n"
            "````\n"
            "code\n"
            "````\n",
        )
        assert any(
            error.startswith("commit message differs from Prettier output")
            for error in lint(long_fence)
        )

        indented_fence = write_message(
            directory,
            "indented-fence.md",
            "checks: treat a four-space-indented fence as code\n\n"
            f"    ```\n    {('long code line ' * 6).rstrip()}\n",
        )
        assert not lint(indented_fence)

        tilde_fenced = write_message(
            directory,
            "tilde-fenced.md",
            "checks: reject Prettier-rewritten tilde fences\n\n~~~\n"
            "code\n"
            "~~~\n",
        )
        assert any(
            error.startswith("commit message differs from Prettier output")
            for error in lint(tilde_fenced)
        )

        indented_code = write_message(
            directory,
            "indented-code.md",
            "checks: allow long indented code lines\n\n"
            f"    {('long code line ' * 6).rstrip()}\n",
        )
        assert not lint(indented_code)

        inline_span = write_message(
            directory,
            "inline-span.md",
            "checks: reject long lines opening with an inline span\n\n"
            f"{('```code``` with more words ' * 5).rstrip()}\n",
        )
        assert "line 3 exceeds 80 characters" in lint(inline_span)

        unterminated = write_message(
            directory,
            "unterminated.md",
            "checks: reject unterminated fences\n\n```\ncode\n",
        )
        assert "fenced code block is never closed" in lint(unterminated)

        unfenced_code = write_message(
            directory,
            "unfenced-code.md",
            "checks: reject rewrappable code lines\n\n"
            f"{('long code line ' * 6).rstrip()}\n",
        )
        assert "line 3 exceeds 80 characters" in lint(unfenced_code)

        invalid_markdown = write_message(
            directory,
            "invalid-markdown.md",
            "checks: show formatting correction\n\n* item\n",
        )
        no_prettier = write_message(
            directory,
            "no-prettier.md",
            "checks: report a missing prettier\n\nBody.\n",
        )
        real_path = os.environ["PATH"]
        os.environ["PATH"] = os.path.join(os.devnull, "missing")
        try:
            prettier_missing = lint(no_prettier)
        finally:
            os.environ["PATH"] = real_path
        assert "prettier is not on PATH" in prettier_missing

        prettier_error = next(
            error
            for error in lint(invalid_markdown)
            if error.startswith("commit message differs from Prettier output")
        )
        assert "--- submitted message" in prettier_error
        assert "+++ Prettier output" in prettier_error
        assert "-* item" in prettier_error
        assert "+- item" in prettier_error


if __name__ == "__main__":
    main()
