# Box Org Synchronization

Home Manager installs rclone and synchronizes `~/org` with `box:org`. A
recursive inotify watcher batches local changes into one sync five minutes after
the first change. A 15-minute timer remains as a fallback and discovers remote
changes. The OAuth-backed `box` remote in `~/.config/rclone/rclone.conf` is
machine-local and must never enter Git or the Nix store.

The services are fail-closed. They require both the machine-local rclone
configuration and `~/.local/state/rclone/org-bisync-ready`, which is created
only after the first bisync has been reviewed and completed manually.

## Fresh-machine bootstrap

Home Manager does not create or store Box credentials. On a new machine, the
installed timer can wake up, but the sync and watcher services skip execution
until their runtime conditions are satisfied. No network or filesystem changes
occur in that state.

Configure the remote interactively and verify its name:

```bash
rclone config
chmod 600 ~/.config/rclone/rclone.conf
rclone listremotes
rclone lsd box:
```

Create a normal user OAuth remote named `box`. Leave the client ID, client
secret, Box application configuration, and advanced settings at their defaults
unless the Box account requires a separately registered application. Never
commit `rclone.conf`: rclone mutates its access and refresh tokens over time.

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
pending batch instead of moving the deadline. After it fires, a new event can
schedule the next batch.

Changes downloaded from Box can schedule one redundant follow-up run. That run
finds no differences and refreshes bisync's listings. The periodic timer also
runs every 15 minutes so remote-only changes and missed filesystem events remain
bounded without relying on inotify.

## Initial synchronization

Create a matching access-check file on both sides, then inspect the initial
resync without changing either side:

```bash
touch ~/org/RCLONE_TEST
rclone copyto ~/org/RCLONE_TEST box:org/RCLONE_TEST
rclone bisync ~/org box:org \
  --filter-from ~/.config/rclone/org-bisync.filter \
  --workdir ~/.cache/rclone/bisync \
  --check-access \
  --check-filename RCLONE_TEST \
  --compare size,modtime,checksum \
  --slow-hash-sync-only \
  --create-empty-src-dirs \
  --resilient \
  --recover \
  --max-lock 30m \
  --resync \
  --resync-mode newer \
  --dry-run \
  --verbose
```

Review every proposed change. Repeat without `--dry-run` only when the result is
correct. After a successful initial resync, create the readiness marker and
start the timer:

```bash
mkdir -p ~/.local/state/rclone
touch ~/.local/state/rclone/org-bisync-ready
systemctl --user start rclone-box-org-bisync.timer
systemctl --user start rclone-box-org-watch.service
```

Do not use `--resync` during normal scheduled operation. It is reserved for the
initial run, deliberate filter changes, or recovery when bisync explicitly
requires it.

## Validation and recovery

```bash
systemctl --user status rclone-box-org-bisync.timer
systemctl --user status rclone-box-org-bisync.service
systemctl --user status rclone-box-org-watch.service
journalctl --user -u rclone-box-org-bisync.service
rclone bisync ~/org box:org \
  --filter-from ~/.config/rclone/org-bisync.filter \
  --workdir ~/.cache/rclone/bisync \
  --check-access \
  --check-filename RCLONE_TEST \
  --compare size,modtime,checksum \
  --slow-hash-sync-only \
  --create-empty-src-dirs \
  --resilient \
  --recover \
  --max-lock 30m \
  --dry-run \
  --verbose
```

If a run reports that resync is required, inspect both sides and perform a
manual `--resync --resync-mode newer --dry-run` before allowing changes. Never
bypass access checks or deletion limits merely to make a failed scheduled run
pass.

See the [rclone Box backend](https://rclone.org/box/) and
[bisync documentation](https://rclone.org/bisync/) for backend limitations and
recovery semantics.
