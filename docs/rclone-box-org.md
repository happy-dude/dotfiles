# Box Org Synchronization

Home Manager installs rclone and synchronizes `~/org` with `box:org`. A
recursive inotify watcher batches local changes into one sync five minutes after
the first change. A 15-minute timer remains as a fallback and discovers remote
changes. The OAuth-backed `box` remote in `~/.config/rclone/rclone.conf` is
machine-local and must never enter Git or the Nix store.

Bisync needs its previous listings to distinguish a deletion from a new file on
the other side. They belong in `$XDG_STATE_HOME/rclone/bisync`, not in a cache
that can be cleared. Activation does not move listings, run a resync or create a
readiness marker. On an existing machine,
[pause both generations and review the baseline](#moving-an-existing-baseline)
before switching.

The sync, watcher and watcher path require the machine-local configuration and
`org-bisync-ready` **inside the state workdir**. They skip execution if the old
`$XDG_STATE_HOME/rclone/org-bisync-ready` still exists, even when both markers
exist. The watcher also needs `~/org`; the path unit starts it once that
directory exists.

## Fresh-machine bootstrap

Home Manager does not create or store Box credentials. On a new machine, the
installed timer can wake up, but the sync and watcher services skip execution
until their runtime conditions are satisfied. No network or filesystem changes
occur in that state.

Configure the remote interactively and verify its name:

```bash
(
  set -eu
  cfg="${XDG_CONFIG_HOME:-$HOME/.config}/rclone/rclone.conf"
  rclone config --config "$cfg"
  chmod 600 "$cfg"
  rclone listremotes --config "$cfg"
  rclone lsd box: --config "$cfg"
)
```

Create a normal user OAuth remote named `box`. Leave the client ID, client
secret, Box application configuration, and advanced settings at their defaults
unless the Box account requires a separately registered application. Never
commit `rclone.conf`: rclone mutates its access and refresh tokens over time.

The service and the manual commands pass the same `--config`, so a different
`RCLONE_CONFIG` cannot select another file. Backend options in the environment
can still override values **in** that file. Before using the remote, make sure
neither the shell nor the user service has overrides such as
`RCLONE_CONFIG_BOX_TYPE` or `RCLONE_BOX_ROOT_FOLDER_ID` pointing elsewhere. Do
not publish environment dumps; they can contain tokens. ref:
[rclone configuration precedence](https://rclone.org/docs/#precedence).

## Synchronized state

The filter at `rclone/org-bisync.filter` excludes:

- the historical `org-roam.bak` tree;
- Org Roam databases and SQLite journals;
- Home Manager's `.dir-locals.el` link;
- Emacs lock files; and
- editor backup files.

The `.org` files and their referenced images are authoritative synchronized
content. Org Roam rebuilds its per-machine database under the local XDG cache.

## Change batching

`rclone-box-org-watch.service` watches `~/org` recursively for closed writes,
creates, deletes, and moves. The first relevant event schedules a transient
timer for five minutes later. Its fixed unit name makes later events reuse that
pending batch instead of moving the deadline. The transient unit only queues the
sync and exits, so a change made while a sync is running schedules the next
batch. That unit is ordered after the bisync service: if a sync is still running
when the batch fires, systemd queues another run after it instead of merging the
request into the running one.

Watcher records use NUL delimiters and filesystem decoding, so embedded or
trailing newlines and non-UTF-8 filename bytes do not change path identity
before exclusion checks. This does not remove inotify's queue and
recursive-watch limitations; the periodic timer remains necessary.

Changes downloaded from Box can schedule one redundant follow-up run. That run
finds no differences and refreshes bisync's listings. The periodic timer also
runs every 15 minutes so remote-only changes and missed filesystem events remain
bounded without relying on inotify.

## Paths for manual runs

Use Bash on the machine being configured. These are the profile paths with the
usual XDG defaults. Check `systemctl --user cat rclone-box-org-bisync.service`;
if its paths differ, use the unit's spellings. The two bisync operands,
including their order, identify the listings. Do not resolve a home-directory
symlink, rename `box:`, or pass the filter's Nix store target instead of its
configuration path.

```bash
org="/home/$(id -un)/org"
conf="${XDG_CONFIG_HOME:-$HOME/.config}/rclone"
state="${XDG_STATE_HOME:-$HOME/.local/state}/rclone"
work="$state/bisync"
old="${XDG_CACHE_HOME:-$HOME/.cache}/rclone/bisync"
ready="$work/org-bisync-ready"
old_ready="$state/org-bisync-ready"
org_bisync() {
  rclone bisync "$org" box:org --config "$conf/rclone.conf" \
    --filters-file "$conf/org-bisync.filter" --workdir "$work" \
    --check-access --check-filename RCLONE_TEST \
    --compare size,modtime,checksum --slow-hash-sync-only \
    --create-empty-src-dirs --resilient --recover \
    --max-delete 50 --max-lock 30m "$@"
}
```

## Moving an existing baseline

**Before activating the new unit**, remove both opt-ins. Conditions do not stop
a running service. Stop the periodic and path triggers, then the watcher, then
any transient timer and service it scheduled. Let an in-flight sync finish; do
not kill it or remove a lock file. Pause any independent manual runs or
schedulers for this pair as well: locks in different workdirs do not exclude one
another. Keep automation disarmed across activation, migration and review. Use
the same pause procedure before manual validation, filter changes or recovery on
an armed machine.

```bash
pause_org() (
  set -eu
  rm -f -- "$ready" "$old_ready"  # local opt-ins only, never RCLONE_TEST
  systemctl --user stop rclone-box-org-bisync.timer rclone-box-org-watch.path
  systemctl --user stop rclone-box-org-watch.service
  systemctl --user stop 'rclone-box-org-bisync-change.*'
  active=$(systemctl --user show -p ActiveState --value rclone-box-org-bisync.service)
  jobs=$(systemctl --user list-jobs --no-legend 'rclone-box-org-*')
  if [[ -n $jobs || ($active != inactive && $active != failed) ]]; then
    printf 'Still busy (%s): %s\nWait for the sync to finish, then repeat.\n' "$active" "$jobs" >&2
    return 1
  fi
)
(
  set -eu
  pause_org
  journalctl --user -u rclone-box-org-bisync.service -n 30 --no-pager
  for dir in "$old" "$work"; do
    if [[ -e $dir || -L $dir ]]; then ls -la -- "$dir"; else printf 'Absent: %s\n' "$dir"; fi
  done
)
```

Stop on any error. Keep private backups outside the active workdirs of the
listings, the filter MD5 (`$conf/org-bisync.filter.md5`) if present, and the
data on both sides. Do not restore readiness markers from backup. Inspect the
last successful run and the complete listing pair, not just directory
timestamps:

- **Cache only:** move the intact session, without renaming individual files.
  The cache is rclone's shared default: if it contains other sessions, leave
  those in place and move only this pair's files, including its recovery files.
- **Both locations:** stop. Do not merge, overwrite, or pick the newest file on
  each side. Retain both snapshots; identify one complete, consistent baseline
  for this exact pair from the last successful run. Set the other one aside
  before placing the reviewed baseline in `"$work"`.
- **State only:** verify its provenance and both listings there; no move is
  needed. **Neither**, or a partial or unidentifiable baseline on a previously
  synced machine, is recovery, not a fresh install. Look for a known-good backup
  before considering a reviewed manual resync.

For the common cache-only case, use this block **only after confirming** that
the old directory is dedicated to this one healthy session, with no lock,
symlinks, or unrelated files. It saves a copy, refuses an existing destination
and verifies the move. On any failure leave both opt-ins absent and inspect the
saved copy and both locations; do not retry by merging directories.

```bash
(
  set -eu
  umask 077
  pause_org
  [[ -d $old && ! -L $old ]]
  [[ ! -e $work && ! -L $work ]]
  [[ ! -e $old/org-bisync-ready && ! -L $old/org-bisync-ready ]]
  mkdir -p -- "$state"
  backup=$(mktemp -d -- "$state/org-bisync-backup.XXXXXXXX")
  printf 'Saving baseline in %s\n' "$backup"
  cp -a -- "$old" "$backup/"
  md5="$conf/org-bisync.filter.md5"
  if [[ -e $md5 || -L $md5 ]]; then
    [[ -f $md5 && ! -L $md5 ]]
    cp -p -- "$md5" "$backup/"
  fi
  diff -qr -- "$old" "$backup/bisync"
  mv -nT -- "$old" "$work"
  [[ ! -e $old ]]
  diff -qr -- "$backup/bisync" "$work"
)
```

Activate the validated generation only when ready. While still disarmed, check
`systemctl --user show -p ExecStart rclone-box-org-bisync.service` for the state
workdir and unchanged operands. Run `org_bisync --dry-run --verbose` **without**
`--resync` against the selected baseline. Review every proposed operation before
[arming the state workdir](#arming-the-state-workdir). If the dry run fails,
keep automation off. If the old setup had no MD5, leave it missing and follow
the [filter-change procedure](#filter-changes); do not manufacture a hash to get
past the check. A resync is not a workdir migration.

For rollback, pause first and leave the old marker absent. An old generation
must not resume from its pre-migration cache after the state baseline has
advanced. Transfer and validate the current baseline in the reverse direction
while disarmed, or leave the older generation's automation off.

## Initial synchronization

Use this only for the **first sync of this pair**. A shared cache may contain
listings for other sessions. Match every entry to its owning pair using those
jobs' operands and run logs; leave unrelated sessions in place. If anything
could belong to Org, even a partial listing or lock, stop and use the migration
procedure. Do not recreate `RCLONE_TEST` to get past an access failure.

The block writes an access-check file to each side, then previews the resync
without applying it. An existing shared cache requires explicit confirmation:

```bash
(
  set -eu
  [[ ! -e $old_ready && ! -L $old_ready ]]
  [[ ! -e $work && ! -L $work && ! -L $old ]]
  if [[ -e $old ]]; then
    [[ -d $old ]]
    ls -la -- "$old"
    read -r -p 'Type UNRELATED only if every entry is accounted for and none belongs to Org: ' answer </dev/tty
    [[ $answer == UNRELATED ]]
  fi
  touch "$org/RCLONE_TEST"
  rclone copyto "$org/RCLONE_TEST" box:org/RCLONE_TEST --config "$conf/rclone.conf"
  org_bisync --resync --resync-mode newer --dry-run --verbose
)
```

Review every proposed change. Only when the result is correct, run
`org_bisync --resync --resync-mode newer --verbose` and check that it succeeds.

## Arming the state workdir

After a successful initial resync or a reviewed normal dry run against a
migrated baseline, explicitly opt in. Starting these units can launch a real
sync. Never create the old marker or arm a baseline with an unresolved error.

```bash
(
  set -eu
  [[ -d $work && ! -L $work && ! -e $old_ready && ! -L $old_ready ]]
  touch "$ready"
  systemctl --user start rclone-box-org-bisync.timer rclone-box-org-watch.path
)
```

Keep `RCLONE_TEST` on both sides permanently. Every scheduled run verifies that
the matching sentinel is visible before applying changes; deleting either copy
causes bisync to fail closed until both copies are restored.

Do not use `--resync` during normal scheduled operation. It is reserved for the
initial run, deliberate filter changes, or recovery when bisync explicitly
requires it.

## Filter changes

`--filters-file` stores the filter's MD5 in
`~/.config/rclone/org-bisync.filter.md5` on each real `--resync`. When the
filter later changes, every run stops with
`filters file has changed (must run --resync)` before touching either side.
Without that stop, files a new rule hides would look deleted and bisync would
delete them on the other side. A machine that last resynced with the earlier
`--filter-from` flag has no hash yet and stops with
`filters file md5 hash not found (must run --resync)`. Do not edit or regenerate
the hash by hand. In both cases, pause automation, review a manual
`org_bisync --resync --resync-mode newer --dry-run --verbose` and repeat it
without `--dry-run` only when safe. Re-arm only after success.

## Validation and recovery

```bash
systemctl --user status rclone-box-org-bisync.timer
systemctl --user status rclone-box-org-bisync.service
systemctl --user status rclone-box-org-watch.path
systemctl --user status rclone-box-org-watch.service
journalctl --user -u rclone-box-org-bisync.service
```

Keep automation paused for any manual bisync, including a dry run: dry runs can
still write working files. With the paths above and both opt-ins absent:

```bash
org_bisync --dry-run --verbose
```

During normal bisync operation, `--max-delete 50` rejects a run when more than
50 percent of the files in either path's prior listing were deleted. This is a
percentage, not a limit of 50 files; renaming a large directory can also trigger
it. If a run reports that resync is required, inspect both sides and perform a
manual `--resync --resync-mode newer --dry-run` before allowing changes. Never
bypass access checks or raise the deletion limit merely to make a failed run
pass.

See the [rclone Box backend](https://rclone.org/box/) and
[bisync documentation](https://rclone.org/bisync/) for backend limitations and
recovery semantics.
