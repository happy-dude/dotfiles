#!/usr/bin/env bash
# Exercise the deployed binding in a real aerc, never the user's account or IPC.
set -euo pipefail
umask 077
aerc=$(realpath "$1")
binds=$(realpath "$2")
root=$(mktemp -d)
cd "$root"

pane() {
  env -i HOME="$root" PATH="$PATH" XDG_RUNTIME_DIR="$root" SHELL="$BASH" \
    LANG=C.UTF-8 tmux -S "$root/socket" -f /dev/null "$@"
}
trap 'pane kill-server 2>/dev/null || :; rm -rf -- "$root"' EXIT
trap 'exit 1' TERM

wait_for() {
  local screen deadline=$((SECONDS + 8))
  while ((SECONDS < deadline)); do
    screen=$(pane capture-pane -p)
    [[ $screen != *"$1"* ]] || return 0
    sleep 0.05
  done
  printf 'aerc did not display %s:\n%s\n' "$1" "$screen" >&2
  return 1
}

mkdir -p mail/INBOX/{cur,new,tmp}
message=mail/INBOX/cur/fixture:2,S
cat >reference <<'MESSAGE'
From: Alice <alice@example.org>
To: Bob <bob@example.org>
Subject: Native deletion fixture
Message-ID: <delete@example.org>
Date: Tue, 18 Nov 2025 16:35:00 +0000

Local fixture body.
MESSAGE
cp reference "$message"
cat >accounts.conf <<ACCOUNT
[fixture]
source=maildir://$root/mail
from=Bob <bob@example.org>
outgoing=$(type -P false)
default=INBOX
ACCOUNT
printf '[general]\n' >aerc.conf
pane new-session -d -x 100 -y 20 "$aerc" -I -a fixture \
  -C "$root/aerc.conf" -A "$root/accounts.conf" -B "$binds"
wait_for 'Native deletion fixture'

for cancel in Enter Escape; do
  pane send-keys d
  wait_for 'delete this message'
  pane send-keys "$cancel"
  wait_for '[fixture] Connected'
  cmp reference "$message"
done
pane send-keys d
wait_for 'delete this message'
pane send-keys y Enter
wait_for '1 message deleted'
test ! -e "$message"
