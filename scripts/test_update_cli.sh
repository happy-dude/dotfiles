#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
UPDATE_SCRIPT="$SCRIPT_DIR/update.sh"

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

help_output="$("$UPDATE_SCRIPT" --help)"
if grep -F -- '--rime-source' <<<"$help_output" >/dev/null; then
  fail 'retired --rime-source option remains in help output'
fi

status=0
legacy_output="$("$UPDATE_SCRIPT" --rime-source nix 2>&1)" || status=$?
[ "$status" -ne 0 ] || fail 'retired --rime-source option was accepted'
grep -F -- 'unknown option: --rime-source' <<<"$legacy_output" >/dev/null ||
  fail 'retired --rime-source option did not use unknown-option rejection'

printf 'Updater CLI tests passed.\n'
