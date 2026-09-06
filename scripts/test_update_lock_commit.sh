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

# A commit failure warns without failing the run. Keep the tip an ordinary
# commit so this exercises the fresh-commit path and its warning text; amend
# and fresh commits fail identically without an identity, on different words.
printf 'lock-v3\n' >"$repo/flake.lock"
printf 'note\n' >"$repo/notes.txt"
git -C "$repo" add notes.txt
git -C "$repo" -c user.name='Update Test' \
  -c user.email='update-test@example.invalid' \
  commit -qm 'tests: an ordinary commit' -- notes.txt
git -C "$repo" config --unset user.name
git -C "$repo" config --unset user.email
commit_flake_lock "$repo" 2>"$TMPDIR_TEST/warn.log" ||
  fail 'commit failure aborted the run'
grep -q 'could not be committed' "$TMPDIR_TEST/warn.log" ||
  fail 'commit failure produced no warning'
test -n "$(git -C "$repo" status --porcelain -- flake.lock)" ||
  fail 'failed commit still marked the lock clean'

# Consecutive unpushed lock updates fold into one commit. The fixture branch
# has no upstream, which is the never-published state.
git -C "$repo" config user.name 'Update Test'
git -C "$repo" config user.email 'update-test@example.invalid'
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" log -1 --format=%s)" = 'nix: update flake.lock' ||
  fail 'the lock update was not committed'
test "$(git -C "$repo" show HEAD:flake.lock)" = lock-v3 ||
  fail 'the lock commit does not carry the refreshed lock'
lock_commits_before=$(git -C "$repo" rev-list --count \
  --grep='^nix: update flake.lock$' HEAD)

printf 'lock-v4\n' >"$repo/flake.lock"
printf 'staged\n' >"$repo/staged.txt"
git -C "$repo" add staged.txt
tip_before_fold=$(git -C "$repo" rev-parse HEAD)
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" rev-parse HEAD)" != "$tip_before_fold" ||
  fail 'second update did not rewrite the lock commit'
test "$(git -C "$repo" show HEAD:flake.lock)" = lock-v4 ||
  fail 'the fold does not carry the newer lock'
test "$(git -C "$repo" rev-list --count --grep='^nix: update flake.lock$' \
  HEAD)" -eq "$lock_commits_before" ||
  fail 'the fold created another lock commit'
test "$(git -C "$repo" status --porcelain -- staged.txt)" = 'A  staged.txt' ||
  fail 'the fold swept a staged file into the lock commit'
test "$(git -C "$repo" rev-parse HEAD^)" = "$(git -C "$repo" rev-parse "$tip_before_fold^")" ||
  fail 'the fold rewrote more than the lock commit'

# An upstream that exists but does not contain the tip still folds.
upstream="$TMPDIR_TEST/upstream.git"
git init -q --bare "$upstream"
git -C "$repo" remote add origin "$upstream"
git -C "$repo" push -q origin 'HEAD^:refs/heads/master'
printf 'lock-v5\n' >"$repo/flake.lock"
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" rev-list --count --grep='^nix: update flake.lock$' \
  HEAD)" -eq "$lock_commits_before" ||
  fail 'an uncontained upstream blocked the fold'
test "$(git -C "$repo" show HEAD:flake.lock)" = lock-v5 ||
  fail 'the uncontained-upstream fold missed the newer lock'

# A lock commit that is already published gets a fresh commit instead.
git -C "$repo" push -q origin 'HEAD:refs/heads/master'
printf 'lock-v6\n' >"$repo/flake.lock"
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" rev-list --count --grep='^nix: update flake.lock$' \
  HEAD)" -eq $((lock_commits_before + 1)) ||
  fail 'a published lock commit was amended in place'
test "$(git -C "$repo" show HEAD:flake.lock)" = lock-v6 ||
  fail 'the published case did not create a fresh lock commit'

# Amend failure warns and does not abort the run.
git -C "$repo" config --unset user.name
git -C "$repo" config --unset user.email
printf 'lock-v7\n' >"$repo/flake.lock"
commit_flake_lock "$repo" 2>"$TMPDIR_TEST/amend-warn.log" ||
  fail 'amend failure aborted the run'
grep -q 'could not be amended' "$TMPDIR_TEST/amend-warn.log" ||
  fail 'amend failure produced no warning'
test -n "$(git -C "$repo" status --porcelain -- flake.lock)" ||
  fail 'failed amend still marked the lock clean'

# The remaining refusals: detached HEAD, a second local branch on the tip.
git -C "$repo" checkout -- flake.lock
git -C "$repo" config user.name 'Update Test'
git -C "$repo" config user.email 'update-test@example.invalid'

printf 'lock-v8\n' >"$repo/flake.lock"
detached_before=$(git -C "$repo" rev-parse HEAD)
git -C "$repo" checkout -q --detach HEAD
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" log -1 --format=%s)" = 'nix: update flake.lock' ||
  fail 'detached HEAD did not get a fresh lock commit'
test "$(git -C "$repo" rev-parse HEAD^)" = "$detached_before" ||
  fail 'detached HEAD rewrote the lock commit'
git -C "$repo" switch -q master

tip_before_branch=$(git -C "$repo" rev-parse HEAD)
printf 'lock-v9\n' >"$repo/flake.lock"
git -C "$repo" branch second-tip
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" rev-parse second-tip)" = "$tip_before_branch" ||
  fail 'fold rewrote a commit another branch names'
test "$(git -C "$repo" rev-parse HEAD)" != "$tip_before_branch" ||
  fail 'a co-located branch did not produce a fresh lock commit'

# A pathspec amend preserves the rest of the commit's tree.
printf 'extra\n' >"$repo/extra.txt"
git -C "$repo" add extra.txt
git -C "$repo" commit -q --amend --no-edit -- extra.txt
printf 'lock-v10\n' >"$repo/flake.lock"
commit_flake_lock "$repo" >/dev/null
test "$(git -C "$repo" show HEAD:extra.txt)" = extra ||
  fail 'the fold lost the rest of the amended tree'

printf 'update lock commit: all cases passed\n'
