import errno
import os
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import host_files
import state_manager


def read_only_source(root: Path) -> Path:
    """Build a source tree with the modes of a Nix store path."""
    source = root / "source"
    (source / "lua").mkdir(parents=True)
    (source / "lua/filter.lua").write_bytes(b"new filter\n")
    (source / "schema.yaml").write_bytes(b"new schema\n")
    for directory, _, files in os.walk(source, topdown=False):
        for name in files:
            Path(directory, name).chmod(0o444)
        Path(directory).chmod(0o555)
    return source


def make_writable(root: Path) -> None:
    for directory, _, _ in os.walk(root):
        Path(directory).chmod(0o755)


class FailureRecoveryTests(unittest.TestCase):
    def test_killed_copy_leaves_staging_that_the_next_run_removes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.addCleanup(make_writable, root)
            source = read_only_source(root)
            live = root / ".home-manager-static"
            live.mkdir()
            (live / "schema.yaml").write_bytes(b"old schema\n")
            # What a kill leaves: copytree has given every finished
            # directory its read-only source mode.
            stale = root / ".home-manager-static.killed00"
            shutil.copytree(source, stale)

            state_manager.refresh_static(source, live)

            self.assertEqual(
                (live / "schema.yaml").read_bytes(), b"new schema\n"
            )
            self.assertEqual(set(root.iterdir()), {source, live})

    def test_killed_swap_restores_backup_and_removes_staging(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.addCleanup(make_writable, root)
            source = read_only_source(root)
            live = root / ".home-manager-static"
            backup = root / ".home-manager-static.home-manager-old"
            backup.mkdir()
            (backup / "schema.yaml").write_bytes(b"old schema\n")
            stale = root / ".home-manager-static.killed00"
            shutil.copytree(source, stale)
            unrelated = root / ".home-manager-static-notes"
            unrelated.write_bytes(b"kept\n")

            state_manager.remove_stale_staging(live, backup)

            self.assertFalse(stale.exists())
            self.assertEqual(
                (backup / "schema.yaml").read_bytes(), b"old schema\n"
            )
            self.assertEqual(unrelated.read_bytes(), b"kept\n")

            state_manager.refresh_static(source, live)
            self.assertEqual(
                (live / "schema.yaml").read_bytes(), b"new schema\n"
            )
            self.assertEqual(set(root.iterdir()), {source, live, unrelated})

    def test_failed_copy_removes_its_read_only_staging(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.addCleanup(make_writable, root)
            source = read_only_source(root)
            live = root / ".home-manager-static"
            live.mkdir()
            (live / "schema.yaml").write_bytes(b"old schema\n")
            copytree = shutil.copytree

            # copytree calls itself for each subdirectory, so the patch sees
            # those calls too; only the outermost one fails.
            def copy_then_fail(*arguments, **options):
                copytree(*arguments, **options)
                if os.fspath(arguments[0]) == os.fspath(source):
                    raise OSError(errno.ENOSPC, "injected copy failure")

            with (
                patch.object(
                    state_manager.shutil,
                    "copytree",
                    side_effect=copy_then_fail,
                ),
                self.assertRaises(OSError) as raised,
            ):
                state_manager.refresh_static(source, live)

            self.assertEqual(raised.exception.errno, errno.ENOSPC)
            self.assertEqual(
                (live / "schema.yaml").read_bytes(), b"old schema\n"
            )
            self.assertEqual(set(root.iterdir()), {source, live})

    def test_failed_install_and_rollback_preserve_backup_for_retry(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            source.mkdir()
            (source / "schema.yaml").write_bytes(b"new schema\n")
            live = root / ".home-manager-static"
            live.mkdir()
            (live / "schema.yaml").write_bytes(b"old schema\n")
            backup = root / ".home-manager-static.home-manager-old"
            replace = os.replace

            def fail_install_and_restore(source_path, target_path):
                if target_path == live:
                    raise OSError(errno.EIO, "injected rename failure")
                replace(source_path, target_path)

            with (
                patch.object(
                    state_manager.os,
                    "replace",
                    side_effect=fail_install_and_restore,
                ),
                self.assertRaises(OSError),
            ):
                state_manager.refresh_static(source, live)

            self.assertFalse(live.exists())
            self.assertEqual(
                (backup / "schema.yaml").read_bytes(), b"old schema\n"
            )
            self.assertEqual(set(root.iterdir()), {source, backup})

            state_manager.refresh_static(source, live)
            self.assertEqual(
                (live / "schema.yaml").read_bytes(), b"new schema\n"
            )
            self.assertFalse(backup.exists())

    def test_failed_static_copy_keeps_schema_links(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "source"
            source.mkdir()
            (source / "schema.yaml").write_bytes(b"old schema\n")
            stamp = root / "stamp"
            stamp.write_bytes(b"stamp-v1\n")
            environment = {
                "XDG_DATA_HOME": str(root / "data"),
                "XDG_STATE_HOME": str(root / "state"),
            }
            link = root / "data/fcitx5/rime/schema.yaml"
            with patch.dict(os.environ, environment):
                state_manager.deploy(source, stamp, "true", ["schema.yaml"])
                self.assertTrue(link.is_symlink())

                (source / "schema.yaml").write_bytes(b"new schema\n")
                stamp.write_bytes(b"stamp-v2\n")
                with (
                    patch.object(
                        state_manager.shutil,
                        "copytree",
                        side_effect=OSError(errno.ENOSPC, "injected"),
                    ),
                    self.assertRaises(OSError),
                ):
                    state_manager.deploy(
                        source, stamp, "true", ["schema.yaml"]
                    )

            self.assertEqual(link.read_bytes(), b"old schema\n")

    def test_failed_host_link_migration_keeps_original_readable(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "declared.conf"
            content = "輸入法設定\n".encode()
            source.write_bytes(content)
            target = root / "runtime.conf"
            target.symlink_to(source)
            snapshot = root / "snapshot"
            host_files.validate_materialize(source, target, snapshot)

            def fail_copy(input_file, output):
                output.write(input_file.read(1))
                raise OSError(errno.ENOSPC, "injected copy failure")

            with (
                patch(
                    "dotfiles_files.shutil.copyfileobj", side_effect=fail_copy
                ),
                self.assertRaises(OSError),
            ):
                host_files.materialize(source, target, snapshot)

            self.assertTrue(target.is_symlink())
            self.assertEqual(target.read_bytes(), content)
            self.assertFalse(snapshot.exists())
            self.assertEqual(set(root.iterdir()), {source, target})

            host_files.materialize(source, target, snapshot)
            self.assertFalse(target.is_symlink())
            self.assertEqual(target.read_bytes(), content)
            self.assertEqual(snapshot.read_bytes(), content)
            self.assertEqual(source.read_bytes(), content)


if __name__ == "__main__":
    unittest.main()
