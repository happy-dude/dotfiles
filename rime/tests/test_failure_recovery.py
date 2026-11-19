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


class StaticRecoveryTests(unittest.TestCase):
    def setUp(self):
        self.root = Path(self.enterContext(tempfile.TemporaryDirectory()))
        self.addCleanup(make_writable, self.root)
        self.enterContext(
            patch.dict(
                os.environ,
                {
                    "XDG_DATA_HOME": str(self.root / "data"),
                    "XDG_STATE_HOME": str(self.root / "state"),
                },
            )
        )
        self.source = read_only_source(self.root)
        self.stamp = self.root / "stamp"
        self.stamp.write_bytes(b"v1")
        self.data, self.live, _, _ = state_manager.state_paths()
        self.deploy()
        self.staging = state_manager.staging(self.live)
        self.backup = self.staging.with_name("previous")
        (self.data / "schema.yaml").write_bytes(b"old schema\n")

    def deploy(self):
        state_manager.deploy(
            self.source, self.stamp, "true", ["schema.yaml", "lua/filter.lua"]
        )

    def test_killed_copy_removes_only_its_own_staging(self):
        # copytree has already applied the read-only store modes. A link in
        # that tree must not cause chmod or rmtree outside it.
        shutil.copytree(self.source, self.staging)
        foreign = self.root / "unrelated"
        foreign.mkdir()
        note = foreign / "note"
        note.write_bytes(b"user data")
        foreign.chmod(0o555)
        self.staging.chmod(0o755)
        (self.staging / "link").symlink_to(foreign)
        self.staging.chmod(0o555)
        # Neither a descriptive backup name nor the old random-name format
        # proves that these directories belonged to an interrupted run.
        for suffix in ("backup", "killed00", "home-manager-old"):
            saved = self.data / (self.live.name + "." + suffix)
            saved.mkdir()
            (saved / "note").symlink_to(self.live / "schema.yaml")
            (saved / "user.yaml").write_bytes(b"personal backup")

        self.stamp.write_bytes(b"v2")
        self.deploy()

        self.assertFalse(self.staging.exists())
        self.assertEqual(
            (self.data / "schema.yaml").read_bytes(), b"new schema\n"
        )
        for suffix in ("backup", "killed00", "home-manager-old"):
            saved = self.data / (self.live.name + "." + suffix)
            self.assertTrue((saved / "note").is_symlink())
            self.assertEqual(
                (saved / "user.yaml").read_bytes(), b"personal backup"
            )
        self.assertEqual(note.read_bytes(), b"user data")
        self.assertEqual(foreign.stat().st_mode & 0o777, 0o555)

    def test_killed_swap_restores_backup_without_losing_runtime_edits(self):
        os.replace(self.live, self.backup)
        shutil.copytree(self.source, self.staging)
        self.deploy()
        self.assertFalse(self.staging.exists())
        self.assertFalse(self.backup.exists())
        self.assertEqual(
            (self.data / "schema.yaml").read_bytes(), b"old schema\n"
        )

    def test_interruption_after_both_renames_restores_stamped_generation(self):
        os.replace(self.live, self.backup)
        shutil.copytree(self.source, self.live)
        self.deploy()
        self.assertFalse(self.backup.exists())
        self.assertEqual(
            (self.data / "schema.yaml").read_bytes(), b"old schema\n"
        )

    def test_removed_schema_retires_only_its_own_link(self):
        visible = self.data / "lua/filter.lua"
        alias = self.data / "user-alias"
        alias.symlink_to(visible.readlink())
        (self.live / "lua/filter.lua").unlink()
        self.assertFalse(visible.exists())
        self.assertTrue(visible.is_symlink())
        (self.source / "lua").chmod(0o755)
        (self.source / "lua/filter.lua").unlink()
        self.stamp.write_bytes(b"v2")
        state_manager.deploy(self.source, self.stamp, "true", ["schema.yaml"])
        self.assertFalse(visible.is_symlink())
        self.assertTrue(alias.is_symlink())
        self.assertEqual(alias.readlink(), self.live / "lua/filter.lua")

    def test_failed_copy_removes_its_read_only_staging(self):
        copytree = shutil.copytree
        self.stamp.write_bytes(b"v2")

        # copytree calls itself for each subdirectory, so the patch sees
        # those calls too; only the outermost one fails.
        def copy_then_fail(*arguments, **options):
            copytree(*arguments, **options)
            if os.fspath(arguments[0]) == os.fspath(self.source):
                raise OSError(errno.ENOSPC, "injected copy failure")

        with (
            patch.object(
                state_manager.shutil, "copytree", side_effect=copy_then_fail
            ),
            self.assertRaises(OSError) as raised,
        ):
            self.deploy()

        self.assertEqual(raised.exception.errno, errno.ENOSPC)
        self.assertFalse(self.staging.exists())
        self.assertEqual(
            (self.data / "schema.yaml").read_bytes(), b"old schema\n"
        )

    def test_failed_install_and_rollback_preserve_backup_for_retry(self):
        self.stamp.write_bytes(b"v2")
        replace = os.replace

        def fail_install_and_restore(source_path, target_path):
            if target_path == self.live:
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
            self.deploy()

        self.assertFalse(self.live.exists())
        self.assertEqual(
            (self.backup / "schema.yaml").read_bytes(), b"old schema\n"
        )
        self.assertFalse(self.staging.exists())
        # A retry may be a rollback to the previous declaration. Its stamp
        # and its edited baseline must still agree after the failed swap.
        self.stamp.write_bytes(b"v1")
        self.deploy()
        self.assertEqual(
            (self.data / "schema.yaml").read_bytes(), b"old schema\n"
        )
        self.stamp.write_bytes(b"v2")
        self.deploy()
        self.assertFalse(self.backup.exists())
        self.assertEqual(
            (self.data / "schema.yaml").read_bytes(), b"new schema\n"
        )


class FailureRecoveryTests(unittest.TestCase):
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
            action = host_files.file_action(source, target, snapshot)

            def fail_copy(input_file, output):
                output.write(input_file.read(1))
                raise OSError(errno.ENOSPC, "injected copy failure")

            with (
                patch(
                    "dotfiles_files.shutil.copyfileobj", side_effect=fail_copy
                ),
                self.assertRaises(OSError),
            ):
                host_files.materialize(source, target, snapshot, action)

            self.assertTrue(target.is_symlink())
            self.assertEqual(target.read_bytes(), content)
            self.assertFalse(snapshot.exists())
            self.assertEqual(set(root.iterdir()), {source, target})

            host_files.materialize(source, target, snapshot, action)
            self.assertFalse(target.is_symlink())
            self.assertEqual(target.read_bytes(), content)
            self.assertEqual(snapshot.read_bytes(), content)
            self.assertEqual(source.read_bytes(), content)


if __name__ == "__main__":
    unittest.main()
