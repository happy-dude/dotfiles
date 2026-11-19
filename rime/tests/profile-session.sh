#!/usr/bin/env bash
# A controller Exit belongs only to this test's daemon, never to a real
# input method. Start the private bus here rather than relying on a caller.
set -euo pipefail

export XDG_RUNTIME_DIR="$PWD/session-run"
mkdir -pm 700 "$XDG_RUNTIME_DIR"
# Use the conventional socket location so the same real daemon is visible
# both through its explicit address and through busctl's --user fallback.
sed "s|<listen>unix:tmpdir=/tmp</listen>|<listen>unix:path=$XDG_RUNTIME_DIR/bus</listen>|" \
  "$2" >session-bus.conf
exec dbus-run-session --config-file="$PWD/session-bus.conf" -- bash -euo pipefail -s -- "$1" "$2" <<'PRIVATE_SESSION'
IFS=$'\n\t'

export HOME="$PWD/session-home"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
mkdir -pm 700 "$XDG_CONFIG_HOME/fcitx5"
source_dir="$PWD/session-source"
cp -r "$1" "$source_dir"
profile="$XDG_CONFIG_HOME/fcitx5/profile"
snapshot="$XDG_STATE_HOME/rime/host-config/profile"
cat >"$profile" <<'EOF'
[Groups/0]
Name=Default
Default Layout=us-colemak

[Groups/0/Items/0]
Name=keyboard-us

[GroupOrder]
0=Default
EOF

controller() {
  busctl --user --auto-start=no --timeout=5s call org.fcitx.Fcitx5 \
    /controller org.fcitx.Fcitx.Controller1 "$@"
}
daemon=
trap 'if [[ -n $daemon ]]; then kill "$daemon" 2>/dev/null || true; wait "$daemon" || true; fi' EXIT
start() {
  fcitx5 -D -k --disable all --enable keyboard,dbus >fcitx.log 2>&1 &
  daemon=$!
  for ((i = 0; i < 100; ++i)); do
    if controller InputMethodGroupInfo s Default >group 2>/dev/null; then
      return
    fi
    kill -0 "$daemon" 2>/dev/null || break
    sleep 0.05
  done
  cat fcitx.log >&2
  return 1
}
stop() {
  controller Exit
  wait "$daemon"
  daemon=
}
host_files() {
  rime-host-files "$1" "$source_dir" "$(command -v busctl)"
}

# Adopt the daemon's own serialization as the managed baseline. That leaves
# only the declaration changing, not a formatting-only three-way conflict.
start
grep -q '"us-colemak"' group
stop
cp "$profile" "$source_dir/profile"
host_files deploy
start
host_files deploy # No replacement required; the daemon may keep running.
sed -i 's/Default Layout=us-colemak/Default Layout=us-dvorak/' "$source_dir/profile"
grep -qx 'Default Layout=us-dvorak' "$source_dir/profile"
for action in validate deploy; do
  if host_files "$action" 2>err; then
    echo 'Accepted a layout change while Fcitx owns the profile' >&2
    exit 1
  fi
  grep -q 'Stop Fcitx' err
  cmp "$profile" "$snapshot"
  grep -qx 'Default Layout=us-colemak' "$snapshot"
done
# The link already shows the new declaration, but the daemon still holds
# the old groups. Do not materialize it or advance the snapshot yet.
mv "$profile" saved-profile
ln -s "$source_dir/profile" "$profile"
for action in validate deploy; do
  if (unset DBUS_SESSION_BUS_ADDRESS; host_files "$action") 2>err; then
    echo 'Accepted a profile link while Fcitx owns the profile' >&2
    exit 1
  fi
  grep -q 'Stop Fcitx' err
  test "$(readlink "$profile")" = "$source_dir/profile"
  cmp saved-profile "$snapshot"
done
# A different, empty session bus is not evidence the desktop bus is idle.
if dbus-run-session --config-file="$2" -- rime-host-files validate \
  "$source_dir" "$(command -v busctl)" 2>err; then
  echo 'A private bus hid the desktop Fcitx instance' >&2
  exit 1
fi
grep -q 'Stop Fcitx' err
mv -f saved-profile "$profile"
stop
host_files deploy
start
grep -q '"us-dvorak"' group
stop
host_files deploy
grep -qx 'Default Layout=us-dvorak' "$profile"
grep -qx 'Default Layout=us-dvorak' "$snapshot"
PRIVATE_SESSION
