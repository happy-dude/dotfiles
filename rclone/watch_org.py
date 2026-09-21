import fnmatch
import os
import subprocess
import sys
from collections.abc import Iterator
from io import BufferedReader
from pathlib import Path


def should_sync(relative: Path) -> bool:
    return not (
        relative.parts[:1] == ("org-roam.bak",)
        or fnmatch.fnmatch(relative.name, "org-roam*.db*")
        or relative.name == ".dir-locals.el"
        or relative.name.startswith(".#")
        or relative.name.endswith("~")
    )


def schedule(systemd_run: str, systemctl: str) -> None:
    # A fixed unit name keeps the first five-minute deadline while later
    # changes join the pending batch.
    subprocess.run(
        [
            systemd_run,
            "--user",
            "--quiet",
            "--collect",
            "--unit=rclone-box-org-bisync-change",
            "--on-active=5m",
            systemctl,
            "--user",
            "start",
            "rclone-box-org-bisync.service",
        ],
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def event_paths(stream: BufferedReader, org_directory: Path) -> Iterator[Path]:
    """Decode NUL-framed paths without changing filename bytes."""
    pending = b""
    while chunk := stream.read1(4096):
        records = (pending + chunk).split(b"\0")
        pending = records.pop()
        for record in records:
            path = Path(os.fsdecode(record))
            try:
                relative = path.relative_to(org_directory)
            except ValueError:
                continue
            yield relative
    if pending:
        raise ValueError("inotifywait ended with an incomplete path record")


def watch(
    org_directory: Path,
    inotifywait: str,
    systemd_run: str,
    systemctl: str,
) -> int:
    with subprocess.Popen(
        [
            inotifywait,
            "--monitor",
            "--recursive",
            "--quiet",
            "--format=%w%f%0",
            "--no-newline",
            "--event=close_write,create,delete,moved_to,moved_from",
            str(org_directory),
        ],
        stdout=subprocess.PIPE,
    ) as watcher:
        try:
            if watcher.stdout is None:
                raise RuntimeError("inotifywait stdout pipe is unavailable")
            for relative in event_paths(watcher.stdout, org_directory):
                if should_sync(relative):
                    schedule(systemd_run, systemctl)
        finally:
            if watcher.poll() is None:
                watcher.terminate()
        return watcher.wait()


def main(arguments: list[str]) -> None:
    if len(arguments) == 2 and arguments[0] == "classify":
        print("sync" if should_sync(Path(arguments[1])) else "ignore")
        return
    if len(arguments) != 5 or arguments[0] != "watch":
        raise SystemExit(
            "usage: rclone-box-org-watch "
            "watch ORG_DIR INOTIFYWAIT SYSTEMD_RUN SYSTEMCTL"
        )
    raise SystemExit(
        watch(
            Path(arguments[1]),
            arguments[2],
            arguments[3],
            arguments[4],
        )
    )


if __name__ == "__main__":
    main(sys.argv[1:])
