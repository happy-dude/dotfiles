#!/usr/bin/env bash

# `nix fmt` evaluates the flake, so it can write flake.lock like any other Nix
# command. --skip-nix-flake promises that the inputs stay locked, so the
# formatter must then refuse to update the lock; before a deliberate
# `nix flake update` it runs unrestricted.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/test-helpers.sh
source "$SCRIPT_DIR/lib/test-helpers.sh"
test_setup

repo="$TMPDIR_TEST/checkout"
fake_bin="$TMPDIR_TEST/bin"
log="$TMPDIR_TEST/commands.log"
create_repo "$repo"
mkdir "$fake_bin"

# Only Nix and Home Manager are substituted; the fake nix records its
# arguments.
# The updater retains every production guard and its supported skip flags.
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/nix"
cat >>"$fake_bin/nix" <<'FAKE'
printf 'nix %s\n' "$*" >>"$UPDATE_TEST_LOG"
FAKE
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/home-manager"
cat >>"$fake_bin/home-manager" <<'FAKE'
[[ $1 == build ]] || exit 92
FAKE
chmod 0755 "$fake_bin/nix" "$fake_bin/home-manager"
export UPDATE_TEST_LOG="$log"

run_update() {
  : >"$log"
  PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
    --skip-pull --skip-submodules --skip-home-manager "$@" "$repo" \
    >"$TMPDIR_TEST/output" 2>&1 ||
    fail "update failed with: $*"
}

run_update --skip-nix-flake
grep -Fqx 'nix fmt --no-update-lock-file .' "$log" ||
  fail 'the formatter could update the lock despite --skip-nix-flake'
if grep -Fq 'nix flake update' "$log"; then
  fail 'updated the inputs despite --skip-nix-flake'
fi

run_update
[[ $(head -n 2 "$log") == $'nix fmt .\nnix flake update' ]] ||
  fail 'the formatter did not run unrestricted before the input update'

printf 'Updater format tests passed.\n'
