#!/usr/bin/env bash

# gitgc.sh rewrites repository storage (prune + gc) in whatever directory it
# is pointed at, so every case runs against a disposable fixture and the
# refusal paths must fire before any git mutation.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
GITGC="$REPO_DIR/scripts/gitgc.sh"

# shellcheck disable=SC1091 # Sourced from the repository.
source "$REPO_DIR/scripts/lib/test-helpers.sh"
test_setup

# Run gitgc expecting failure and require the expected explanation.
assert_refuses() {
  local description=$1
  local expected=$2
  shift 2
  local output status=0

  output="$(bash "$GITGC" "$@" 2>&1)" || status=$?
  ((status != 0)) || fail "$description: expected a non-zero exit"
  grep -F -- "$expected" <<<"$output" >/dev/null ||
    fail "$description: expected '$expected', got: $output"
}

help_output="$(bash "$GITGC" --help)"
grep -F -- 'Usage:' <<<"$help_output" >/dev/null ||
  fail 'help output does not describe usage'

assert_refuses 'unknown option' 'Unknown option: --bogus' --bogus

plain_dir="$TMPDIR_TEST/plain"
mkdir -p -- "$plain_dir"
assert_refuses 'non-repository' 'not a git repository' "$plain_dir"

# A repository without an origin remote skips the prune and still collects.
no_origin="$TMPDIR_TEST/no-origin"
create_repo "$no_origin"
no_origin_output="$(bash "$GITGC" "$no_origin")"
grep -F -- 'No origin remote configured' <<<"$no_origin_output" >/dev/null ||
  fail "origin-less repository did not skip the prune: $no_origin_output"
grep -F -- 'Done!' <<<"$no_origin_output" >/dev/null ||
  fail "origin-less repository did not finish: $no_origin_output"
[ -n "$(find "$no_origin/.git/objects/pack" -name '*.pack' -print -quit)" ] ||
  fail 'gc produced no pack file in the origin-less repository'

# A stale remote-tracking branch is pruned while live ones survive.
upstream="$TMPDIR_TEST/upstream.git"
git init -q --bare --initial-branch=main "$upstream"
pruned_repo="$TMPDIR_TEST/pruned"
create_repo "$pruned_repo"
git -C "$pruned_repo" remote add origin "$upstream"
git -C "$pruned_repo" push -q -u origin HEAD:refs/heads/main
git -C "$pruned_repo" push -q origin HEAD:refs/heads/stale
git -C "$upstream" update-ref -d refs/heads/stale
git -C "$pruned_repo" rev-parse --verify -q refs/remotes/origin/stale \
  >/dev/null || fail 'fixture lost its stale remote-tracking branch early'
prune_output="$(bash "$GITGC" "$pruned_repo")"
grep -F -- 'Pruning stale remote-tracking branches' <<<"$prune_output" \
  >/dev/null || fail "prune phase missing: $prune_output"
if git -C "$pruned_repo" rev-parse --verify -q refs/remotes/origin/stale \
  >/dev/null; then
  fail 'stale remote-tracking branch survived gitgc'
fi
git -C "$pruned_repo" rev-parse --verify -q refs/remotes/origin/main \
  >/dev/null || fail 'live remote-tracking branch was pruned'

# An initialized submodule is garbage-collected along with its parent.
child_repo="$TMPDIR_TEST/child"
create_repo "$child_repo"
parent_repo="$TMPDIR_TEST/parent"
create_repo "$parent_repo"
git -C "$parent_repo" -c protocol.file.allow=always submodule add -q \
  "$child_repo" child
commit_all "$parent_repo" 'add child submodule'
submodule_output="$(bash "$GITGC" "$parent_repo")"
grep -F -- 'Processing: child' <<<"$submodule_output" >/dev/null ||
  fail "submodule was not traversed: $submodule_output"
[ -n "$(
  find "$parent_repo/.git/modules/child/objects/pack" -name '*.pack' -print -quit
)" ] || fail 'gc produced no pack file in the child submodule'

# A second positional directory is refused before any mutation.
assert_refuses 'two directories' 'Multiple directories specified' \
  "$no_origin" "$pruned_repo"

# An unreachable origin warns and still garbage-collects.
git -C "$no_origin" remote add origin "$TMPDIR_TEST/nonexistent.git"
offline_output="$(bash "$GITGC" "$no_origin" 2>&1)"
grep -F -- 'Warning: could not prune origin' <<<"$offline_output" >/dev/null ||
  fail "unreachable origin did not warn: $offline_output"
grep -F -- 'Done!' <<<"$offline_output" >/dev/null ||
  fail "unreachable origin stopped the run: $offline_output"

# --aggressive is accepted and still reaches every phase.
aggressive_output="$(bash "$GITGC" --aggressive "$parent_repo")"
grep -F -- 'Running aggressive gc' <<<"$aggressive_output" >/dev/null ||
  fail "aggressive run did not announce itself: $aggressive_output"
grep -F -- 'Processing: child' <<<"$aggressive_output" >/dev/null ||
  fail "aggressive run skipped the submodule: $aggressive_output"
grep -F -- 'Done!' <<<"$aggressive_output" >/dev/null ||
  fail "aggressive run did not finish: $aggressive_output"

printf 'gitgc: all cases passed\n'
