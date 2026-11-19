"""Check the deployed aerc inputs without starting a live account."""

import difflib
import shlex
import subprocess
import sys
from configparser import ConfigParser
from fnmatch import fnmatchcase
from pathlib import Path


def load(path: Path) -> ConfigParser:
    # Global binds precede the first section; d/D are distinct keys. Strict
    # parsing rejects duplicate filters rather than treating them as fallbacks.
    parser = ConfigParser(delimiters=("=",), interpolation=None)
    parser.optionxform = str
    parser.read_string("[global]\n" + path.read_text(), source=str(path))
    return parser


def settings(config: ConfigParser) -> list[str]:
    result = []
    for section in sorted(config.sections()):
        items = list(config[section].items())
        if section != "filters":
            items.sort()
        result.extend(f"[{section}] {k}={v}" for k, v in items)
    return result


def check_choice(binding: str, command: str) -> None:
    args = shlex.split(binding.removesuffix("<Enter>"))
    if not (
        binding.endswith("<Enter>")
        and len(args) == 5
        and args[:3] == [":choose", "-o", "y"]
        and args[-1] == command
    ):
        raise ValueError(
            f"expected an affirmative {command} choice: {binding}"
        )


def main() -> None:
    mirror, actual, binds, accounts, directory = map(Path, sys.argv[1:])
    deployed = load(actual)
    diff = list(
        difflib.unified_diff(
            settings(load(mirror)),
            settings(deployed),
            fromfile=str(mirror),
            tofile=str(actual),
            lineterm="",
        )
    )
    if diff:
        raise SystemExit("\n".join(diff))

    # Select the first matching MIME glob in file order, then use the same
    # shell pipeline and packaged filters that aerc passes the part body to.
    filters = deployed["filters"]
    key = next(key for key in filters if fnmatchcase("text/html", key))
    if key != "text/html":
        raise ValueError(f"text/html is shadowed by {key}")
    rendered = subprocess.run(
        ["sh", "-c", filters[key]],
        input="<html><body><p>Local &amp; readable</p></body></html>\n",
        stdout=subprocess.PIPE,
        text=True,
        check=True,
    ).stdout
    if "Local & readable" not in rendered or "<p>" in rendered:
        raise ValueError(f"HTML filter returned {rendered!r}")

    keys = load(binds)
    for section, key in [
        ("global", "<C-c>"),
        ("global", "<C-q>"),
        ("messages", "q"),
    ]:
        check_choice(keys[section][key], "quit")
    for section, key in [("messages", "d"), ("messages", "D"), ("view", "D")]:
        check_choice(keys[section][key], "delete-message")
    for key, command in [("<C-Up>", "prev"), ("<C-Down>", "next")]:
        assert keys["messages"][key] == f":{command} 100%<Enter>"

    account = load(accounts)["dev"]
    assert account["source"] == "notmuch://"
    assert Path(account["query-map"]) == directory / "notmuch-map.conf"


if __name__ == "__main__":
    main()
