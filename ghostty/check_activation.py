"""Check installed launch commands without a shell's Nix PATH."""

import configparser
import shlex
import subprocess
import sys
import tempfile
from pathlib import Path


def main() -> None:
    package = Path(sys.argv[1])
    service = Path(sys.argv[2])
    executable = str(package / "bin/ghostty")
    resources = {
        package / "share/applications/com.mitchellh.ghostty.desktop": [
            ("Desktop Entry", "TryExec"),
            ("Desktop Entry", "Exec"),
            ("Desktop Action new-window", "Exec"),
        ],
        package / "share/dbus-1/services/com.mitchellh.ghostty.service": [
            ("D-BUS Service", "Exec"),
        ],
        service: [("Service", "ExecStart")],
    }
    with tempfile.TemporaryDirectory() as home:
        environment = {
            "HOME": home,
            "PATH": "/no-profile-bin",
            "LANG": "C.UTF-8",
        }
        for resource, entries in resources.items():
            # systemd permits repeated directives, such as Unit.After.
            config = configparser.ConfigParser(
                interpolation=None, strict=resource != service
            )
            with resource.open() as stream:
                config.read_file(stream)
            for section, key in entries:
                command = shlex.split(config[section][key])
                if not command or command[0] != executable:
                    raise AssertionError(
                        f"{resource}: {section}.{key} must launch "
                        f"{executable}, got {command!r}"
                    )
                subprocess.run(
                    [*command, "--version"],
                    env=environment,
                    stdout=subprocess.DEVNULL,
                    check=True,
                    timeout=30,
                )
    print("Desktop, D-Bus and systemd commands run without a profile PATH")


if __name__ == "__main__":
    main()
