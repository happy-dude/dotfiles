# Fedora Kinoite Determinate Nix: upgrade and rollback

This document holds the rollback, retirement, and major Fedora upgrade
procedures for the native Determinate Nix setup. It complements the reusable
[installation runbook](fedora-kinoite-determinate-nix.md); dated host history
lives in the [worldmind migration history](fedora-kinoite-migration-history.md).

## Rollback and recovery

Use the narrowest rollback layer that addresses the failure:

1. **Boot failure or missing `/nix`:** choose the pinned prior OSTree deployment
   in the bootloader. Do not repair a failed mount by making the global root
   writable.
2. **User-profile failure:** inspect the cutover backup and current profile
   targets before restoring anything. Restore only links that resolve into the
   active `/nix/store` namespace; do not recreate the retired Toolbx environment
   as a profile fallback.
3. **Native-store damage:** preserve the failed store and use the separately
   recorded tested backup procedure. Do not improvise a direct Btrfs subvolume
   snapshot or restore at `/var/home/nix`.
4. **Daemon or SELinux failure:** capture `findmnt`, unit definitions and
   ordering, boot journals, labels, and AVCs. Fix the specific ordering or
   policy regression; do not disable SELinux.
5. **Installer removal:** the installed interface is
   `nix-installer uninstall [OPTIONS] [RECEIPT]`, defaulting to
   `/nix/receipt.json`. Preserve the receipt and review the current `--help`
   before use. The available `--no-confirm` option is intentionally not part of
   this runbook. Uninstall and active-store deletion require a separate
   destructive-operation plan.

Never delete `/var/home/nix`, the pinned deployment, its tested backup, or
cutover recovery artifacts merely to retry an installation.

## Retired Toolbx state

The retirement gate is complete: native Nix survived multiple cold boots and a
later Fedora deployment, the full native updater passed, application smoke tests
passed, container-only state and submodule stashes were inventoried, and tested
backup and rollback paths exist.

The `nix-toolbox-42` container and `ghcr.io/thrix/nix-toolbox:42` image were
removed on 2026-07-10. Do not recreate them as a first-line repair path; use the
pinned deployment, native-store backup, and cutover artifacts instead.

Keep the pinned deployment until after the first successful major-upgrade
postflight. Treat legacy-store cleanup and Determinate installer removal as
independent destructive operations that each require explicit approval.

## Major Fedora upgrade checklist

### Release-note gate

Before every major Fedora upgrade, retrieve and review the current
target-version material. Do not rely only on this Fedora 44 record.

Review, at minimum:

- Fedora Kinoite/Atomic Desktop upgrade documentation and release notes.
- Fedora Change proposals affecting composefs, rpm-ostree, bootc, SELinux,
  systemd, util-linux, and Nix.
- OSTree release notes and the current `ostree-prepare-root` documentation.
- bootc filesystem documentation for `root.transient-ro`.
- Determinate release notes, issue #1445, and the current OSTree planner source.
- The Fedora Nix package source and README, in case Fedora now provides an
  Atomic-safe `/nix` solution.
- Home Manager and Nixpkgs release notes for the locked inputs.
- Release notes for any adopted third-party Flatpak Home Manager module.

Record the URLs, access date, target versions, and conclusions in the upgrade
record template below. Secondary blogs, gists, and discussion posts may identify
problems, but validate their commands against target-version primary sources and
the live system.

### Preflight

1. Fully update and reboot the current Fedora release first.
2. Confirm no rpm-ostree transaction is active.
3. Pin the known-good booted deployment.
4. Capture:
   - `rpm-ostree status` and `sudo ostree admin status`;
   - `/etc/ostree/prepare-root.conf` and `rpm-ostree initramfs-etc`;
   - OSTree, util-linux, systemd, installer, Nix, and SELinux versions;
   - `systemctl cat` and ordering for every helper/Determinate Nix unit;
   - root and `/nix` mounts, owners, modes, SELinux labels, and unit hashes;
   - build-user UID/GID allocations and the loaded Nix SELinux module;
   - `/nix/receipt.json` and the installer binary checksum, excluding secrets;
   - Home Manager generation, a locked build, repository status, and disk space;
   - Flatpak user/system remotes, applications, and overrides.
5. Stop or finish active Nix builds and garbage collection.
6. Determine the actual Btrfs layout with `btrfs subvolume show /var/home/nix`.
   Select and test a backup or snapshot method appropriate to that layout; do
   not invent a snapshot command before checking it.
7. Verify the pinned deployment is available in the bootloader.

### Upgrade execution

Use the exact official Fedora Kinoite upgrade command published for the target
release at that time. Do not copy a future rebase reference into this Fedora 44
record. Review the staged deployment and reboot normally.

### Post-upgrade

Before running Home Manager switch or deleting a rollback deployment, verify:

1. The intended deployment and kernel are booted.
2. `/` remains read-only.
3. The prepare-root override remains tracked in the initramfs.
4. `/nix` mounts from `/var/home/nix`.
5. The helper runs before `nix.mount`; daemon and sockets are healthy.
6. Nix store ownership, build identities, SELinux labels, and policy are intact.
7. No Nix-related AVC denial appears during `nix store info` and a real build.
8. A locked `nix flake check` and Home Manager build pass before activation.
9. Fresh Bash and Fish logins resolve the intended Nix client.
10. Ghostty, tmux modified keys, Rime persistence, bgutil, Git signing, Solaar,
    and Flatpak applications pass smoke tests.
11. A second reboot produces the same healthy state.

A missing `/nix`, unexpectedly writable global root, failed helper/mount/daemon,
Nix-related SELinux denial, store corruption, or wrong shell Nix client is a
rollback condition. Stop and boot the pinned deployment rather than improvising
a destructive repair.

### Upgrade record template

```text
Date:
Current Fedora deployment:
Target Fedora deployment:
Current kernel / target kernel:
OSTree / util-linux / systemd versions:
Determinate installer / Nix versions:
Reviewed release notes and access dates:
Helper still required? Evidence:
prepare-root checksum before / after:
helper unit checksum before / after:
/nix mount source before / after:
SELinux module and AVC result:
Locked Home Manager build result:
Application smoke-test result:
Pinned rollback deployment:
Backup location and restore test:
Decision to retain or retire rollback state:
```
