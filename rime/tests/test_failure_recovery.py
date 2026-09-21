import errno
import os
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import host_files
import state_manager


class FailureRecoveryTests(unittest.TestCase):
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
