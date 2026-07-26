# Fedora Kinoite Determinate Nix: worldmind migration history

This document records the dated, host-specific `worldmind` migration to native
Determinate Nix. It is deliberately separate from the reusable
[installation runbook](fedora-kinoite-determinate-nix.md) and the
[upgrade and rollback procedure](fedora-kinoite-upgrade-rollback.md) so that one
machine's history does not churn the reusable material.

## Worldmind native deployment record

As of 2026-07-10, `worldmind` runs Determinate Nix directly on Fedora Kinoite.
`/var/home/nix` is the persistent store behind the host-visible `/nix` mount,
and Home Manager uses `nixPackage = null` so it does not install a competing Nix
client. The former `nix-toolbox-42` container and `ghcr.io/thrix/nix-toolbox:42`
image are retired; `toolbox list` is empty.

Any content remaining at `~/.local/share/nix` is inactive legacy data, not part
of the active Nix or Home Manager profile and not a valid recovery source.
Legacy-store cleanup remains separate from container and image retirement.

`/var/home/nix` is a normal directory on the existing `/home` Btrfs subvolume,
not a subvolume of its own. Back it up by copying the directory or snapshotting
an appropriate containing subvolume. Running
`btrfs subvolume snapshot /var/home/nix` is invalid on this installation.

Recorded recovery artifacts:

- Installer-time profile backup:
  `~/.local/state/nix-toolbox-profile-backup-20260710-100258`
- Native Home Manager cutover backup:
  `~/.local/state/nix-native-cutover-20260710-110632`
- Quarantined copied launchers, icons, and legacy unit links:
  `~/.local/state/nix-native-cutover-20260710-110632/quarantined-after-native-home-manager`
- Stash cleanup archive refs:
  `refs/archive/worldmind-stash-cleanup/20260710T202927-0500`
- Stash cleanup manifest and repository bundles:
  `~/.local/state/worldmind-stash-cleanup-20260710T202927-0500`

The stash archive covers all 599 pre-cleanup stashes. Normal stash refs retain
the six substantive entries; 593 verified empty or generated-payload entries
were removed only after the archive refs and repository bundles were created.

Native postflight confirms:

- Determinate Nix resolves in fresh Bash and Fish logins, and Home Manager
  contains no competing Nix client.
- `/nix` mounts from persistent home storage while the composefs root remains
  read-only.
- The daemon, sockets, mountpoint helper, and SELinux policy survive multiple
  cold boots and a later Fedora deployment.
- `./scripts/update.sh --verbose` completes natively, including flake checks, a
  locked Home Manager build, and activation. Its generic submodule stage is a
  no-op when `.gitmodules` has no entries.
- Ghostty, tmux Shift+Enter, Rime learned state, bgutil, Git signing, and user
  Flatpak smoke tests pass.
- Copied Ghostty integration and obsolete Home Manager unit links remain
  quarantined in the cutover backup.
