# Fedora Kinoite with native Determinate Nix

This runbook records the native Nix installation on `worldmind` and the checks
needed to reproduce or maintain it. The host is Fedora Kinoite with
OSTree/composefs, not NixOS. Home Manager manages the user environment; Fedora
continues to own the operating system, Flatpak, desktop portals, and host
services.

> **Validated:** 2026-07-10 on Fedora Kinoite 44.20260710.0, x86_64. The
> commands and pinned installer below are an installation record, not a promise
> that future Fedora, OSTree, systemd, util-linux, or Determinate releases retain
> the same interfaces. Re-run the major-upgrade review before carrying this
> setup into another Fedora release.

## Command labels

- **🟦 RUN DIRECTLY ON THE KINOITE HOST — FISH-COMPATIBLE** means paste the
  block into an interactive Fish terminal on `worldmind`.
- **🟨 SAVE AS A BASH SCRIPT, THEN RUN ON THE KINOITE HOST** means save the
  complete fenced block as the named file and execute it with `bash`. Do not
  paste the surrounding prose into the shell.
- **🟧 RUN INSIDE THE LEGACY NIX TOOLBOX** is used only for rollback inventory.
  Fresh installations skip those steps.

Commands that stage an OSTree deployment, write `/etc`, enable system units,
install Nix, switch Home Manager, reboot, or remove rollback state are explicitly
marked as state-changing.

## Validated architecture

| Layer                     | Owner and state                                                                     |
| ------------------------- | ----------------------------------------------------------------------------------- |
| Operating system          | Fedora Kinoite 44, deployed through rpm-ostree with a composefs root                |
| Root filesystem           | Read-only for ordinary processes                                                    |
| Dynamic `/nix` mountpoint | Created transiently during boot by `nix-ostree-mountpoint.service`                  |
| Persistent Nix data       | `/var/home/nix`, mounted at `/nix` by Determinate's `nix.mount`                     |
| Nix implementation        | Determinate Nix installer 3.21.5, Nix 2.34.8                                        |
| SELinux                   | Enforcing, with the installer-provided Nix policy loaded                            |
| User configuration        | Generic-Linux Home Manager output `.#schan`, not NixOS                              |
| Nix package selection     | `nixPackage = null`; Home Manager must not install a competing Nix client           |
| Rollback during migration | Pinned OSTree deployment plus the preserved Toolbox, old store, and profile backups |

The validated snapshot also recorded:

- libostree 2026.2, util-linux 2.41.5, and systemd 259.7;
- `/etc/ostree/prepare-root.conf` SHA-256
  `b26eae204bcff095db0c3486428a56256ae940dda4334e8cbf700d52ee080ee2`;
- the read-only capture SHA-256
  `ba40e63bf4a153a3f69f314c6a5d2a250f5652873763a139c62838f6f60fc97e`.

The composefs root cannot create `/nix` through the installer planner's legacy
`chattr` path. OSTree's `root.transient-ro` feature allows a privileged helper
to create the mountpoint in a private mount namespace while the global root
stays read-only. Determinate then mounts persistent `/var/home/nix` at that
mountpoint and starts its daemon.

The helper is local compatibility glue. As of the validated installer release,
[Determinate issue #1445](https://github.com/DeterminateSystems/nix-installer/issues/1445)
remains open and the OSTree planner still needs this assistance on Fedora's
composefs Atomic desktops. Re-evaluate the helper whenever the installer,
OSTree, util-linux, systemd, or Fedora major version changes.

## Safety invariants

Do not continue an installation or upgrade unless all of these remain true:

1. A known-good OSTree deployment is pinned before changing early-boot state.
2. `/` is read-only outside the helper's private mount namespace.
3. Persistent Nix data lives only at `/var/home/nix`; `/nix` is its mountpoint.
4. UIDs/GIDs 30000 through 30032 do not collide with existing identities.
5. SELinux remains enforcing; do not solve Nix failures by disabling it.
6. The Determinate install plan selects the OSTree planner, Determinate Nix, and
   `/var/home/nix` persistence.
7. A locked Home Manager build succeeds before any live switch.
8. Existing Toolbox storage and profile backups remain intact until native Nix
   survives multiple boots and a Fedora deployment update.
9. Destructive retirement and installer uninstall are separate, explicitly
   approved operations—not troubleshooting shortcuts.

## Sources and support boundary

Primary references:

- [bootc dynamic mountpoints with `transient-ro`](https://bootc.dev/bootc/filesystem.html#dynamic-mountpoints-with-transient-ro)
- [OSTree `prepare-root` manual](https://ostreedev.github.io/ostree/man/ostree-prepare-root.html)
- [Determinate workstation installation](https://docs.determinate.systems/getting-started/individuals/)
- [Determinate OSTree planner merge](https://github.com/DeterminateSystems/nix-installer/pull/586)
- [Determinate composefs incompatibility](https://github.com/DeterminateSystems/nix-installer/issues/1445)
- [Fedora 44 Nix package change](https://fedoraproject.org/wiki/Changes/Nix_package_tool)
- [Fedora Nix RPM sources](https://src.fedoraproject.org/rpms/nix)
- [rpm-ostree administrator handbook](https://coreos.github.io/rpm-ostree/administrator-handbook/)

The Fedora 44 Nix RPM is useful on conventional Fedora, but Fedora's own change
page still calls `/nix` incompatible with rpm-ostree and recommends Toolbox or a
rootless mode there. This installation therefore uses Determinate's OSTree
planner plus the local mountpoint helper. Community posts and gists are useful
leads, but they are not the authority for a future Fedora release.

The NixOS `services.flatpak` module is also not applicable: this is standalone
Home Manager on Kinoite. The separately pinned `nix-flatpak` Home Manager module
converges the declared user installation, while Fedora continues to provide the
Flatpak executable and portals. Its Home Manager service cannot manage or
remove system-scoped refs.

## 1. Preflight a fresh or migrating host

### 🟦 RUN DIRECTLY ON THE KINOITE HOST — FISH-COMPATIBLE

This block is read-only except for `sudo` credential validation:

```fish
sudo -v

printf '\n== Host and deployment ==\n'
hostnamectl
cat /etc/os-release
uname -a
rpm-ostree status
sudo ostree admin status

printf '\n== Filesystems ==\n'
findmnt -T /
findmnt -T /var/home
readlink -f /home
readlink -f "$HOME"

if test -e /nix
    ls -ldZ /nix
    findmnt -T /nix
else
    echo '/nix is absent'
end

printf '\n== Existing Nix state ==\n'
for path in \
        /var/home/nix \
        /etc/nix \
        /etc/nix-installer \
        /etc/determinate \
        /usr/local/bin/determinate-nixd \
        /etc/systemd/system/nix-directory.service \
        /etc/systemd/system/nix.mount \
        /etc/systemd/system/nix-daemon.service \
        /etc/systemd/system/nix-daemon.socket \
        /etc/systemd/system/determinate-nixd.socket
    if sudo test -e "$path"; or sudo test -L "$path"
        sudo ls -ldZ "$path"
    else
        echo "absent: $path"
    end
end

printf '\n== UID/GID collisions ==\n'
for id in (seq 30000 30032)
    getent passwd "$id"
    getent group "$id"
end

printf '\n== SELinux and space ==\n'
getenforce
sestatus
df -hT /var/home

printf '\n== Required commands ==\n'
for command_name in unshare mount install semodule restorecon
    printf '%-12s ' "$command_name"
    command -v "$command_name"; or echo MISSING
end
```

Expected properties, rather than exact output:

- The booted deployment is healthy and can be pinned.
- `/` is the read-only composefs/overlay deployment root.
- `/var/home` is persistent and has enough capacity for a Nix store.
- A fresh host has no `/nix`, `/var/home/nix`, Determinate units, or Nix build
  identities.
- SELinux is enforcing and all required commands are present.

If migrating from `nix-toolbox`, also inventory the container, its mounts, the
old store, profile links, and dirty repository/Rime state before proceeding.
Do not delete or reuse the old store.

## 2. Enable OSTree `transient-ro`

This step changes `/etc`, pins the current deployment, stages a new OSTree
deployment with the override in its initramfs, and requires a reboot.

The tested Fedora vendor file contained the composefs and read-only sysroot
settings. Preserve those vendor settings; add only the `[root]` override. Abort
and review manually if `/etc/ostree/prepare-root.conf` already exists or the
vendor file already has a `[root]` section.

The final validated override was:

```ini
[composefs]
enabled = yes
[sysroot]
readonly = true

[root]
transient-ro = true
```

`rpm-ostree initramfs-etc` reported this file as tracked. Its validated SELinux
label was `system_u:object_r:etc_t:s0`.

### 🟨 SAVE AS A BASH SCRIPT, THEN RUN ON THE KINOITE HOST

Save as `~/Downloads/enable-kinoite-transient-ro.sh`:

```bash
#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'

vendor=/usr/lib/ostree/prepare-root.conf
override=/etc/ostree/prepare-root.conf
temporary=$(mktemp)

cleanup() {
    rm -f "$temporary"
}
trap cleanup EXIT

fail() {
    printf 'ERROR: %s\n' "$*" >&2
    exit 1
}

sudo -v
[[ -r $vendor ]] || fail "missing vendor configuration: $vendor"
sudo test ! -e "$override" || fail "override already exists: $override"
grep -Eq '^\[composefs\]$' "$vendor" || fail 'vendor composefs section is missing'
grep -Eq '^\[sysroot\]$' "$vendor" || fail 'vendor sysroot section is missing'
! grep -Eq '^\[root\]$' "$vendor" || fail 'vendor root section requires manual review'

sudo ostree admin pin 0
cp -- "$vendor" "$temporary"
printf '\n[root]\ntransient-ro = true\n' >>"$temporary"
sudo install -D -o root -g root -m 0644 "$temporary" "$override"
sudo rpm-ostree initramfs-etc --track=/etc/ostree/prepare-root.conf

sudo sed -n '1,200p' "$override"
rpm-ostree initramfs-etc
rpm-ostree status
```

Run the saved script:

```bash
bash "$HOME/Downloads/enable-kinoite-transient-ro.sh"
```

Review the staged deployment, then reboot explicitly:

### 🟦 RUN DIRECTLY ON THE KINOITE HOST — FISH-COMPATIBLE

```fish
systemctl reboot
```

After reboot, confirm that the new deployment is booted, the override is active,
root is still read-only, and `/nix` remains absent before installing the helper:

```fish
rpm-ostree status
sudo ostree admin status
findmnt -T / -o TARGET,SOURCE,FSTYPE,OPTIONS
sudo sed -n '1,200p' /etc/ostree/prepare-root.conf
rpm-ostree initramfs-etc

test ! -e /nix
and echo 'OK: /nix is absent before the helper runs'
```

## 3. Create `/nix` before Determinate's mount unit

The helper uses the documented `transient-ro` pattern: it unshares a mount
namespace, remounts the deployment root writable only inside that namespace, and
creates `/nix`. Determinate's later `nix.mount` supplies the persistent mount.

`LIBMOUNT_FORCE_MOUNT2=always` is a compatibility shim for the tested util-linux
release, not part of OSTree's general `transient-ro` contract. Re-test whether it
is still needed after a util-linux or kernel change.

### 🟨 SAVE AS A SYSTEMD UNIT, THEN INSTALL ON THE KINOITE HOST

Save this exact file as
`~/Downloads/nix-ostree-mountpoint.service`:

```systemd
[Unit]
Description=Create the transient /nix mount point on OSTree
Documentation=https://bootc.dev/bootc/filesystem.html#dynamic-mountpoints-with-transient-ro
DefaultDependencies=no
ConditionPathExists=!/nix
Requires=local-fs-pre.target
After=local-fs-pre.target
Before=nix-directory.service nix.mount

[Service]
Type=oneshot
RemainAfterExit=yes
Environment=LIBMOUNT_FORCE_MOUNT2=always
ExecStart=/usr/bin/unshare --mount --propagation private -- /bin/sh -eu -c '/usr/bin/mount -o remount,rw / && /usr/bin/install -d -o root -g root -m 0755 /nix'

[Install]
RequiredBy=nix-directory.service nix.mount
```

Install, verify, and enable it. This changes `/etc/systemd/system` and starts the
helper:

### 🟦 RUN DIRECTLY ON THE KINOITE HOST — FISH-COMPATIBLE

```fish
set helper_source "$HOME/Downloads/nix-ostree-mountpoint.service"
set helper_target /etc/systemd/system/nix-ostree-mountpoint.service

sudo install -D -o root -g root -m 0644 "$helper_source" "$helper_target"
sudo systemd-analyze verify "$helper_target"
sudo systemctl daemon-reload
sudo systemctl enable --now nix-ostree-mountpoint.service

systemctl is-enabled nix-ostree-mountpoint.service
systemctl is-active nix-ostree-mountpoint.service
sudo systemctl status nix-ostree-mountpoint.service --no-pager

sudo find /etc/systemd/system \
    -maxdepth 3 \
    -type l \
    -name '*nix-ostree-mountpoint.service' \
    -printf '%p -> %l\n'
```

The enable operation creates requirements from both future installer unit names
to the helper. On the validated host those links were:

- `/etc/systemd/system/nix-directory.service.requires/nix-ostree-mountpoint.service`
- `/etc/systemd/system/nix.mount.requires/nix-ostree-mountpoint.service`

Before running the Determinate installer, these checks must pass:

```fish
systemctl is-active nix-ostree-mountpoint.service
ls -ldZ /nix
not mountpoint -q /nix
and echo 'OK: /nix exists but is not mounted yet'

set root_options (findmnt -nro OPTIONS -T /)
string match -rq '(^|,)ro(,|$)' -- "$root_options"
and echo 'OK: the global root remains read-only'
```

Warnings that `nix-directory.service` and `nix.mount` do not yet exist are
expected when the helper is enabled before the installer creates them. The
required dependency symlinks must nevertheless exist afterward.

## 4. Install pinned Determinate Nix

The validated migration retained Determinate's default telemetry behavior and
used the following exact installer artifact:

- Installer: `nix-installer 3.21.5`
- Architecture: `x86_64-linux`
- SHA-256: `ee9c560d6f093baf7a8b342d8a00e9f8b47dd4a6367f3f523482ee96897c4179`
- Persistence: `/var/home/nix`
- Distribution: Determinate Nix, not upstream Nix

For a future installation, check the current Determinate release notes and
artifact checksum first. Do not silently reuse this checksum for another
version.

### 🟨 SAVE AS A BASH SCRIPT, THEN RUN ON THE KINOITE HOST

Save as `~/Downloads/install-determinate-nix-ostree.sh`:

```bash
#!/usr/bin/env bash
set -Eeuo pipefail
IFS=$'\n\t'

version=3.21.5
expected_sha256=ee9c560d6f093baf7a8b342d8a00e9f8b47dd4a6367f3f523482ee96897c4179
url="https://github.com/DeterminateSystems/nix-installer/releases/download/v${version}/nix-installer-x86_64-linux"
installer="$HOME/Downloads/nix-installer-v${version}-x86_64-linux"
temporary="${installer}.download"

cleanup() {
    rm -f "$temporary"
}
trap cleanup EXIT

fail() {
    printf 'ERROR: %s\n' "$*" >&2
    exit 1
}

sudo -v
[[ $(uname -m) == x86_64 ]] || fail "expected x86_64, found $(uname -m)"
sudo systemctl is-active --quiet nix-ostree-mountpoint.service ||
    fail 'the OSTree mountpoint helper is not active'
[[ -d /nix ]] || fail '/nix does not exist'
mountpoint -q /nix && fail '/nix is already mounted'
sudo test ! -e /var/home/nix || fail '/var/home/nix already exists'

root_options=$(findmnt -nro OPTIONS -T /)
case ",${root_options}," in
    *,ro,*) ;;
    *) fail "the global root is not read-only: ${root_options}" ;;
esac

for dependency in \
    /etc/systemd/system/nix-directory.service.requires/nix-ostree-mountpoint.service \
    /etc/systemd/system/nix.mount.requires/nix-ostree-mountpoint.service
do
    [[ -L $dependency ]] || fail "missing dependency link: ${dependency}"
done

curl \
    --proto '=https' \
    --tlsv1.2 \
    --fail \
    --show-error \
    --location \
    --output "$temporary" \
    "$url"
printf '%s  %s\n' "$expected_sha256" "$temporary" |
    sha256sum --check --strict -
install -m 0755 "$temporary" "$installer"
[[ $("$installer" --version) == "nix-installer ${version}" ]] ||
    fail 'unexpected installer version'

printf '%s\n' \
    'Review the interactive plan before accepting it.' \
    'Abort unless it selects the OSTree planner, Determinate Nix,' \
    'and persistence at /var/home/nix.'

sudo --set-home env \
    -u ORIG_HOME \
    -u DETSYS_IDS_TELEMETRY \
    -u NIX_INSTALLER_DIAGNOSTIC_ENDPOINT \
    -u NIX_INSTALLER_PREFER_UPSTREAM_NIX \
    -u NIX_INSTALLER_NO_CONFIRM \
    -u NIX_INSTALLER_MODIFY_PROFILE \
    -u NIX_SENTRY_ENDPOINT \
    "$installer" install ostree \
    --determinate \
    --persistence /var/home/nix \
    --explain

mountpoint -q /nix || fail '/nix is not mounted after installation'
findmnt --mountpoint /nix -o TARGET,SOURCE,FSTYPE,OPTIONS
sudo ls -ldZ /var/home/nix /nix /nix/store /nix/var/nix/db
getenforce
sudo semodule -l | grep -E '^nix([[:space:]]|$)' ||
    fail 'the Nix SELinux policy is not installed'
/nix/var/nix/profiles/default/bin/nix --version
sudo test -x /nix/nix-installer || fail '/nix/nix-installer is missing'
sudo test -f /nix/receipt.json || fail '/nix/receipt.json is missing'
```

Run the saved script:

```bash
bash "$HOME/Downloads/install-determinate-nix-ostree.sh"
```

The installer creates `/var/home/nix`, `nix.mount`, daemon and socket units,
Nix build identities, the SELinux policy, `/nix/receipt.json`, and
`/usr/local/bin/determinate-nixd`. It does not need to replace an existing
Toolbox `~/.nix-profile` during the system installation.

Reboot once more and validate boot ordering before bootstrapping Home Manager.

## 5. Validate the native service after reboot

### 🟦 RUN DIRECTLY ON THE KINOITE HOST — FISH-COMPATIBLE

```fish
rpm-ostree status
findmnt -T / -o TARGET,SOURCE,FSTYPE,OPTIONS
findmnt --mountpoint /nix -o TARGET,SOURCE,FSTYPE,OPTIONS

for unit in \
        nix-ostree-mountpoint.service \
        nix-directory.service \
        nix.mount \
        ensure-symlinked-units-resolve.service \
        nix-daemon.socket \
        determinate-nixd.socket \
        nix-daemon.service
    printf '\n--- %s ---\n' "$unit"
    systemctl show "$unit" \
        -p LoadState \
        -p UnitFileState \
        -p ActiveState \
        -p SubState \
        -p Result \
        -p ConditionResult
end

/nix/var/nix/profiles/default/bin/nix --version
/nix/var/nix/profiles/default/bin/nix store info --store daemon
getenforce
sudo semodule -lfull | rg '(^|[[:space:]])nix([[:space:]]|$)'
sudo journalctl -b \
    -u nix-ostree-mountpoint.service \
    -u nix-directory.service \
    -u nix.mount \
    -u nix-daemon.socket \
    -u determinate-nixd.socket \
    -u nix-daemon.service \
    --no-pager
```

Expected properties:

- `/` remains read-only.
- `/nix` is mounted from the persistent `/home/nix` Btrfs path represented by
  `/var/home/nix` on the host.
- The helper, mount, daemon socket, Determinate socket, and daemon are healthy.
- `nix-directory.service` may be skipped with `ConditionPathExists=!/nix`
  because the helper already created the mountpoint. That is expected.
- The daemon store responds. A non-trusted user warning about a restricted
  client `store` setting is not a failed store check.
- Determinate authentication or update-check warnings are non-blocking when the
  machine is not logged in to FlakeHub; daemon health is evaluated separately.
- No Nix-related SELinux denial appears during a real build.

## 6. Bootstrap this Home Manager configuration

The `schan` output deliberately sets `nixPackage = null`. `nix/default.nix`
installs only the user flake configuration and locked registry, while the flake
exports its locked Home Manager package as `.#home-manager` for first use.

A fresh installation can clone normally. A Toolbox migration must first back up
and quarantine the old user profile namespace; see the migration record below.

### 🟦 RUN DIRECTLY ON THE KINOITE HOST — FISH-COMPATIBLE

```fish
cd "$HOME/dotfiles"
set native_nix /nix/var/nix/profiles/default/bin/nix

"$native_nix" \
    --extra-experimental-features 'nix-command flakes' \
    flake check \
    --show-trace \
    --no-update-lock-file

"$native_nix" \
    --extra-experimental-features 'nix-command flakes' \
    run .#home-manager -- \
    build \
    --flake .#schan \
    --show-trace \
    --no-out-link \
    --no-update-lock-file
```

Review the build. The first switch changes the live user profile:

```fish
"$native_nix" \
    --extra-experimental-features 'nix-command flakes' \
    run .#home-manager -- \
    switch \
    --flake .#schan \
    --show-trace \
    --no-update-lock-file
```

After activation, a fresh Bash and Fish login must resolve the Determinate
client from `/nix/var/nix/profiles/default/bin/nix`; the Home Manager profile
must not contain `~/.nix-profile/bin/nix`.

## 7. Application postflight

### Native Nix and Home Manager

```fish
type -a nix
readlink -f (type -p nix)
test ! -e "$HOME/.nix-profile/bin/nix"
and echo 'OK: Home Manager does not contain a competing Nix client'

nix store info --store daemon
home-manager generations | head -n 5
cd "$HOME/dotfiles"
./scripts/update.sh check
```

`update.sh check` is the final locked repository validation. It does not update
`flake.lock` or activate a new profile.

### Git filesystem monitoring and inotify

Home Manager explicitly disables Git's built-in fsmonitor while retaining the
untracked cache. With fsmonitor enabled globally, a recursive traversal of this
submodule-heavy checkout starts a detached `git fsmonitor--daemon` for each
initialized repository. Multiplying that daemon across hundreds of submodules
can consume the per-user inotify capacity shared with desktop applications.
Disabling fsmonitor addresses the repository shape without increasing a host
sysctl.

After migrating from a profile that enabled fsmonitor, stop its daemons once:

```fish
cd "$HOME/dotfiles"

git -c core.fsmonitor=true fsmonitor--daemon stop; or true
git submodule foreach --quiet --recursive \
    'git -c core.fsmonitor=true fsmonitor--daemon stop >/dev/null 2>&1 || :'

kde-inotify-survey | jq '.totals'
```

The survey is user-scoped and does not need `sudo`. Do not accept KDE's
persistent sysctl increase merely to accommodate per-submodule Git daemons. If
instance use remains near the warning threshold after the daemons stop and a
fresh login, inspect the survey's per-process instance and watch counts before
changing `fs.inotify.max_user_instances` or `fs.inotify.max_user_watches`.

### Ghostty and tmux

The Toolbox launcher and copied icons are obsolete after native activation. The
native profile must contain Ghostty, and the removed `ghostty-toolbox` desktop
entry must be absent. tmux uses Ghostty's `extkeys` capability and CSI-u so
applications can request modified-key reporting.

```fish
ghostty --version

test ! -e "$HOME/.local/share/applications/ghostty-toolbox.desktop"
and test ! -L "$HOME/.local/share/applications/ghostty-toolbox.desktop"
and echo 'OK: copied Toolbox launcher is absent'

test ! -e "$HOME/.nix-profile/share/applications/ghostty-toolbox.desktop"
and echo 'OK: active profile contains no Toolbox launcher'

set tmux_socket "native-key-check-$fish_pid"
env TERM=xterm-ghostty \
    tmux -L "$tmux_socket" \
    -f "$HOME/.config/tmux/tmux.conf" \
    new-session -d -s validation

tmux -L "$tmux_socket" show-options -s extended-keys
tmux -L "$tmux_socket" show-options -s extended-keys-format
tmux -L "$tmux_socket" show-options -s terminal-features |
    rg 'xterm-ghostty:RGB:extkeys'
tmux -L "$tmux_socket" kill-server
```

For the end-to-end key test, open a new native Ghostty window, start a fresh tmux
client, run Codex, type one line, press Shift+Enter, and type a second line. The
unsent prompt must retain both lines. Existing clients may predate the feature
negotiation and are not a valid test.

The profile-provided desktop entry and user service start nixGL-wrapped Ghostty
without forcing software rendering. Before launching Fish, its configured
command removes graphics override variables so terminal children use their
own host, Nix, or Flatpak graphics integration. The Mesa wrapper is the correct
primary-GPU path for worldmind's integrated Intel graphics; Ghostty does not
need PRIME offload or a Vulkan wrapper.

### Rime

Rime needs no immediate native-host rewrite. Keep the ownership checks and the
`.home-manager-static` materialization: it now separates immutable managed
schemas from writable generated, learned, and sync state rather than crossing a
Toolbox-only store boundary.

Before any later simplification:

1. Determine whether the active Rime directory is a real directory or a legacy
   Stow symlink.
2. Record all `*.userdb`, `sync/`, and `user.yaml` locations.
3. Type with several active schemas.
4. Add a harmless learned candidate, restart Fcitx, and confirm it persists.
5. Reboot and repeat before moving any state.

### bgutil yt-dlp provider

No host bridge or listening service is required. Home Manager points yt-dlp at
the locked plugin, native Node runtime, and one-shot provider script in the
native store. The validated provider version was 1.3.1. Avoid enabling the
optional HTTP service: the locked upstream can listen on non-loopback addresses.

### Flatpak and other host integration

Native `/nix` does not bypass Flatpak sandboxing. Fedora remains responsible for
Flatpak and Plasma portals. The pinned `nix-flatpak` v0.7.0 Home Manager module
declares user applications in `flatpak/default.nix`. It leaves unmanaged apps,
unused runtimes, existing overrides, and updates untouched. A Home Manager
activation installs missing declarations but never changes the system Flatpak
installation.

Keep `services.flatpak.overrides` empty while using nix-flatpak v0.7.0: its
merge serializer can introduce leading empty permissions into externally managed
list entries, which Flatpak 1.18 cannot parse safely. Keep global Fcitx,
filesystem, and session-bus overrides host-managed, and isolate wrapped
application graphics state at the launcher boundary instead.

qView is declared from Flathub. Firefox Nightly is declared through the
commit-pinned upstream `firefox-nightly.flatpakref`, which records its `master`
branch, `firefoxnightly-origin` remote, repository URL, and signing key. Do not
replace that descriptor with a raw remote URL or a nonexistent nix-flatpak
`branch` option. Stable Firefox uses the different `org.mozilla.firefox` ID and
is not a substitute for `org.mozilla.FirefoxNightly`.

The qView and Firefox Nightly system-to-user migration is deliberately staged:

1. Close both applications and back up their existing `~/.var/app` directories.
2. Record system and user refs, remotes, overrides, permissions, and commits.
3. Activate Home Manager while retaining the system refs.
4. Test the new copies explicitly with
   `flatpak run --user com.interversehq.qView` and
   `flatpak run --user org.mozilla.FirefoxNightly`. Do not run both Firefox
   Nightly scopes simultaneously against the shared profile.
5. Confirm qView preferences and Firefox bookmarks, extensions, and
   `about:profiles` state. Both scopes use the same application-ID-keyed paths
   below `~/.var/app`; no profile copy is expected when the ID is unchanged.
6. Only after validation, explicitly uninstall each system ref without
   `--delete-data`. Keep remote and unused-runtime cleanup as a separate audit.

While both scopes exist, unqualified `flatpak run` selects the user ref first.
Always pass `--user` or `--system` during migration, and never use
`--delete-data`: it would remove the application-ID-keyed user state and
permission-store entries shared by the migration.

Keep `dev.edfloreshz.CosmicTweaks` unmanaged during the native migration. Remove
this user-scoped Flatpak explicitly only after application migration is complete
and the later host/Toolbox cleanup begins; verify its data and overrides before
uninstalling it.

Keep the nixGL package input and per-application wrappers on generic Linux even
though Nix is native; they bridge Nix-built GUI applications to the host
graphics stack without altering the package set through a nixGL overlay. On
worldmind, Home Manager owns the wrapped Solaar executable and user autostart
entry. The host retains only an explicit `solaar-udev` RPM overlay because
generic-Linux Home Manager cannot activate udev rules from the Nix store.

## Existing Toolbox migration record

The migration on 2026-07-10 started with Nix and Home Manager inside
`nix-toolbox-42`, a user-owned 33 GiB store at `~/.local/share/nix`, and profile
links into that store. The container and store were deliberately retained as a
rollback layer.

`/var/home/nix` was confirmed to be a normal directory on the existing `/home`
Btrfs subvolume, not a subvolume of its own. A future backup plan must therefore
back up the directory or snapshot an appropriate containing subvolume; running
`btrfs subvolume snapshot /var/home/nix` would be invalid on this installation.

The repository changes were split into four logical commits:

1. Select host-provided Nix for `schan` and export the locked Home Manager
   bootstrap package.
2. Remove Ghostty Toolbox desktop integration and host-copy activation.
3. Let each host provide its own Nix path instead of hardcoding the native
   installer profile in shared Fish configuration.
4. Enable request-driven tmux CSI-u extended keys for Ghostty.

Before the first native Home Manager switch, the old profile namespace was
quarantined and the repository diff, staged diff, lock checksum, and Rime user
state inventory were recorded. The native switch then created a clean Home
Manager generation without a Nix client. Git comparisons using a neutral global
configuration proved that apparent status differences came from the restored
Git ignore configuration rather than activation changes.

Recorded rollback locations:

- Installer-time profile backup:
  `~/.local/state/nix-toolbox-profile-backup-20260710-100258`
- Native Home Manager cutover backup:
  `~/.local/state/nix-native-cutover-20260710-110632`
- Quarantined copied launchers, icons, and legacy unit links:
  `~/.local/state/nix-native-cutover-20260710-110632/quarantined-after-native-home-manager`

The first native verification confirmed:

- Determinate Nix resolved in fresh Bash and Fish logins.
- Home Manager and its activation contained no competing Nix client.
- `/nix` mounted from persistent home storage and root stayed read-only.
- Daemon, sockets, helper, SELinux policy, Ghostty, and Rime links were healthy.
- The Toolbox launcher, five copied Ghostty icons, and six obsolete dangling
  Home Manager unit links were quarantined rather than deleted.
- bgutil required no migration-specific host changes.

## Rollback and recovery

Use the narrowest rollback layer that addresses the failure:

1. **Boot failure or missing `/nix`:** choose the pinned prior OSTree deployment
   in the bootloader. Do not repair a failed mount by making the global root
   writable.
2. **Before the first Home Manager switch:** stop and leave the Toolbox profile,
   store, and container untouched.
3. **User-profile failure after the switch:** inspect the cutover backup and
   current profile targets before restoring anything. Confirm which `/nix` a
   Toolbox sees now that the host has a native mount; do not assume the old
   fallback still resolves correctly.
4. **Daemon or SELinux failure:** capture `findmnt`, unit definitions and
   ordering, boot journals, labels, and AVCs. Fix the specific ordering or policy
   regression; do not disable SELinux.
5. **Installer removal:** the installed interface is `nix-installer uninstall
[OPTIONS] [RECEIPT]`, defaulting to `/nix/receipt.json`. Preserve the receipt
   and review the current `--help` before use. The available `--no-confirm`
   option is intentionally not part of this runbook. Uninstall and store
   deletion are destructive retirement operations requiring a separate plan.

Never delete `/var/home/nix`, the old Toolbox store, pinned deployment, or
cutover backups merely to retry an installation.

## Toolbox retirement gate

Retire the Toolbox only after all of these have happened:

- Native Nix has survived multiple cold boots.
- At least one new Fedora deployment has booted with the helper ordering intact.
- `./scripts/update.sh check` and a separately approved `apply` pass natively.
- Ghostty, tmux Shift+Enter, Rime learned state, bgutil, Git signing, and Flatpak
  smoke tests pass.
- `podman diff`, container-only packages/configuration, submodule stashes, and
  old profile/store backups have been inventoried.
- A tested rollback path and a backup of `/var/home/nix` exist.

Stop the container first and observe normal operation. Remove the container and
image later. Delete the old 33 GiB store only as the final, separately confirmed
step. Keep the pinned deployment until after the first successful major-upgrade
postflight.

## Major Fedora upgrade checklist

### Release-note gate

Before every major Fedora upgrade, retrieve and review the current target-version
material. Do not rely only on this Fedora 44 record.

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
6. Determine the actual Btrfs layout with `btrfs subvolume show
/var/home/nix`. Select and test a backup or snapshot method appropriate to
   that layout; do not invent a snapshot command before checking it.
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
