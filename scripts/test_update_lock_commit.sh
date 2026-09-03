#!/usr/bin/env bash

# The lock commit is the only step that turns a validated update into a
# committed result; every case runs against a fixture repository.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"

# shellcheck disable=SC1091 # Sourced from the repository.
source "$REPO_DIR/scripts/lib/test-helpers.sh"
test_setup

# shellcheck disable=SC1091 # Intentionally sources the repository script.
source "$REPO_DIR/scripts/update.sh"

repo="$TMPDIR_TEST/repo"
create_repo "$repo"
printf 'lock-v1\n' >"$repo/flake.lock"
commit_all "$repo" 'nix: initial lock'
git -C "$repo" config user.name 'Update Test'
git -C "$repo" config user.email 'update-test@example.invalid'

# A validated update leaves a dirty lock; the helper commits just it.
printf 'lock-v2\n' >"$repo/flake.lock"
printf 'unrelated\n' >"$repo/unrelated"
commit_flake_lock "$repo"
git -C "$repo" log -1 --format='%s' | grep -qx 'nix: update flake.lock' ||
  fail 'lock update was not committed'
test -z "$(git -C "$repo" status --porcelain -- flake.lock)" ||
  fail 'flake.lock still dirty after the commit'
test -n "$(git -C "$repo" status --porcelain -- unrelated)" ||
  fail 'unrelated file was swept into the commit'
test "$(git -C "$repo" show --stat --format='' HEAD | grep -c '|')" -eq 1 ||
  fail 'commit touched more than flake.lock'

# A clean lock is a no-op.
before="$(git -C "$repo" rev-parse HEAD)"
commit_flake_lock "$repo"
test "$(git -C "$repo" rev-parse HEAD)" = "$before" ||
  fail 'clean lock created a commit'

# A commit failure warns without failing the run.
printf 'lock-v3\n' >"$repo/flake.lock"
git -C "$repo" config --unset user.name
git -C "$repo" config --unset user.email
commit_flake_lock "$repo" 2>"$TMPDIR_TEST/warn.log" ||
  fail 'commit failure aborted the run'
grep -q 'could not be committed' "$TMPDIR_TEST/warn.log" ||
  fail 'commit failure produced no warning'
test -n "$(git -C "$repo" status --porcelain -- flake.lock)" ||
  fail 'failed commit still marked the lock clean'

printf 'update lock commit: all cases passed\n'
