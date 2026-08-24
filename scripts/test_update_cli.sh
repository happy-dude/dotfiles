#!/usr/bin/env bash

# The updater's argument handling decides whether a run mutates the checkout,
# so every rejection here must happen before any step runs. Modes that would
# perform work are never invoked; only the paths that refuse are exercised.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
UPDATE_SCRIPT="$SCRIPT_DIR/update.sh"

# shellcheck disable=SC1091 # Sourced from the repository.
source "$SCRIPT_DIR/lib/test-helpers.sh"

# Run the updater and require it to refuse with the expected explanation.
assert_rejects() {
  local description=$1
  local expected=$2
  shift 2
  local output status=0

  output=$(bash "$UPDATE_SCRIPT" "$@" 2>&1) || status=$?
  ((status != 0)) || fail "$description: expected a non-zero exit"
  grep -F -- "$expected" <<<"$output" >/dev/null ||
    fail "$description: expected '$expected', got: $output"
}

help_output="$(bash "$UPDATE_SCRIPT" --help)"
grep -F -- 'Usage:' <<<"$help_output" >/dev/null ||
  fail 'help output does not describe usage'
if grep -F -- '--rime-source' <<<"$help_output" >/dev/null; then
  fail 'retired --rime-source option remains in help output'
fi

assert_rejects 'retired option' 'unknown option: --rime-source' \
  --rime-source nix
assert_rejects 'unknown option' 'unknown option: --nonsense' --nonsense

# A mode is a mode, not a directory: naming two must be refused rather than
# silently treating the second as somewhere to operate.
assert_rejects 'two modes' 'multiple modes specified: check and apply' \
  check apply
assert_rejects 'two directories' \
  'multiple repository directories specified: /tmp and /var' \
  check /tmp /var

# Activation cannot be skipped in the mode whose purpose is to activate.
assert_rejects 'apply without activation' \
  'apply mode cannot be combined with --skip-home-manager' \
  apply --skip-home-manager

printf 'Updater CLI tests passed.\n'
