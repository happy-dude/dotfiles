#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# Neutralise the user's global Git configuration: the managed commit-msg
# hook lints every commit, so a suite that creates them must not depend on
# whether that hook is installed on the machine running it.
export GIT_CONFIG_GLOBAL=/dev/null

source_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
sync_script="$source_root/scripts/sync-local-branch.sh"
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT
fake_bin="$temporary_directory/fake-bin"
mkdir "$fake_bin"

printf '#!%s\n' "$(command -v bash)" >"$fake_bin/nix"
cat >>"$fake_bin/nix" <<'EOF'
set -euo pipefail
printf 'nix|%s|%s\n' "$PWD" "$*" >>"$SYNC_TEST_LOG"
if [[ ${1:-} == fmt && ${SYNC_TEST_FORMAT_UNTRACKED:-false} == true ]]; then
  touch formatter-output
fi
if [[ ${1:-} == flake && ${SYNC_TEST_FAIL_CHECK:-false} == true ]]; then
  exit 23
fi
EOF
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/home-manager"
cat >>"$fake_bin/home-manager" <<'EOF'
set -euo pipefail
printf 'home-manager|%s|%s\n' "$PWD" "$*" >>"$SYNC_TEST_LOG"
EOF
chmod 0755 "$fake_bin/nix" "$fake_bin/home-manager"
export PATH="$fake_bin:$PATH"

create_fixture() {
  local name=$1

  fixture_root="$temporary_directory/$name"
  origin="$fixture_root/origin.git"
  seed="$fixture_root/seed"
  main_worktree="$fixture_root/main worktree"
  branch_worktree="$fixture_root/work tree"
  output="$fixture_root/output"
  log="$fixture_root/commands.log"
  mkdir -p "$fixture_root"
  : >"$log"
  export SYNC_TEST_LOG="$log"
  unset SYNC_TEST_FORMAT_UNTRACKED SYNC_TEST_FAIL_CHECK

  git init --quiet --bare "$origin"
  git init --quiet --initial-branch=main "$seed"
  git -C "$seed" config user.name "Sync Test"
  git -C "$seed" config user.email sync@example.invalid
  printf '%s\n' base >"$seed/shared"
  git -C "$seed" add shared
  git -C "$seed" commit --quiet -m 'tests: create sync base'
  git -C "$seed" remote add origin "$origin"
  git -C "$seed" push --quiet -u origin main
  git -C "$origin" symbolic-ref HEAD refs/heads/main

  git clone --quiet "$origin" "$main_worktree"
  git -C "$main_worktree" config user.name "Sync Test"
  git -C "$main_worktree" config user.email sync@example.invalid
  git -C "$main_worktree" branch work
  git -C "$main_worktree" worktree add --quiet "$branch_worktree" work
  git -C "$branch_worktree" config user.name "Sync Test"
  git -C "$branch_worktree" config user.email sync@example.invalid
}

advance_origin() {
  local path=$1
  local contents=$2

  printf '%s\n' "$contents" >"$seed/$path"
  git -C "$seed" add "$path"
  git -C "$seed" commit --quiet -m "tests: advance origin $path"
  git -C "$seed" push --quiet origin main
}

commit_work() {
  local path=$1
  local contents=$2

  printf '%s\n' "$contents" >"$branch_worktree/$path"
  git -C "$branch_worktree" add "$path"
  git -C "$branch_worktree" commit --quiet -m "tests: add work $path"
}

run_sync() {
  (
    cd -- "$fixture_root"
    bash "$sync_script" "$@"
  ) >"$output" 2>&1
}

create_fixture success
commit_work work-only local
advance_origin upstream-only upstream
unrelated_worktree="$fixture_root/unrelated worktree"
git -C "$main_worktree" branch unrelated
git -C "$main_worktree" worktree add --quiet \
  "$unrelated_worktree" unrelated
touch "$unrelated_worktree/untracked"
remote_head=$(git -C "$origin" rev-parse refs/heads/main)
git -C "$main_worktree" config --unset-all remote.origin.fetch
git -C "$main_worktree" config --add remote.origin.fetch \
  '+refs/heads/unrelated:refs/remotes/origin/unrelated'
run_sync work test-profile "$main_worktree"
test "$(git -C "$main_worktree" rev-parse main)" = "$remote_head"
test "$(git -C "$main_worktree" rev-parse origin/main)" = "$remote_head"
test "$(git -C "$branch_worktree" merge-base work origin/main)" = \
  "$remote_head"
test "$(git -C "$origin" rev-parse refs/heads/main)" = "$remote_head"
grep -Fq "nix|$branch_worktree|fmt ." "$log"
grep -Fq \
  "nix|$branch_worktree|flake check --show-trace --no-update-lock-file" \
  "$log"
grep -Fq \
  "home-manager|$branch_worktree|build --flake .#test-profile --show-trace --no-out-link --no-update-lock-file" \
  "$log"
grep -Fq 'Nothing was pushed. The named branch remains local.' "$output"

create_fixture upstream-equivalent
commit_work equivalent same-change
equivalent_commit=$(git -C "$branch_worktree" rev-parse HEAD)
commit_work work-only unique-change
advance_origin equivalent same-change
run_sync work test-profile "$main_worktree"
grep -Fq \
  'Local commits already represented upstream; rebase is expected to omit:' \
  "$output"
equivalent_summary=$(git -C "$branch_worktree" show -s \
  --format='%h %s' "$equivalent_commit")
grep -Fq "  $equivalent_summary" "$output"
test "$(git -C "$branch_worktree" rev-list --count origin/main..work)" = 1
test "$(<"$branch_worktree/equivalent")" = same-change
test "$(<"$branch_worktree/work-only")" = unique-change

create_fixture dirty-main
advance_origin upstream-only upstream
old_tracking=$(git -C "$main_worktree" rev-parse origin/main)
touch "$main_worktree/untracked"
if run_sync work test-profile "$main_worktree"; then
  printf 'synchronized with a dirty main worktree\n' >&2
  exit 1
fi
test "$(git -C "$main_worktree" rev-parse origin/main)" = "$old_tracking"
grep -Fq '?? untracked' "$output"
grep -Fq 'main worktree is not clean' "$output"
grep -Fq "Dirty worktree: $main_worktree" "$output"
grep -Fq 'preflight stopped before fetch or mutation' "$output"

create_fixture dirty-work
advance_origin shared upstream
old_tracking=$(git -C "$main_worktree" rev-parse origin/main)
printf '%s\n' upstream >"$branch_worktree/shared"
if run_sync work test-profile "$main_worktree"; then
  printf 'synchronized with a dirty work worktree\n' >&2
  exit 1
fi
test "$(git -C "$main_worktree" rev-parse origin/main)" = "$old_tracking"
grep -Fq ' M shared' "$output"
grep -Fq 'work worktree is not clean' "$output"
grep -Fq "Dirty worktree: $branch_worktree" "$output"
grep -Fq 'preflight stopped before fetch or mutation' "$output"

create_fixture conflict
commit_work shared local
advance_origin shared upstream
remote_head=$(git -C "$origin" rev-parse refs/heads/main)
if run_sync work test-profile "$main_worktree"; then
  printf 'completed a conflicting rebase\n' >&2
  exit 1
fi
test "$(git -C "$main_worktree" rev-parse main)" = "$remote_head"
test -d "$(git -C "$branch_worktree" rev-parse \
  --path-format=absolute --git-path rebase-merge)"
printf -v expected_continue 'git -C %q rebase --continue' "$branch_worktree"
printf -v expected_abort 'git -C %q rebase --abort' "$branch_worktree"
printf -v expected_validate '  %q --validate %q %q %q' \
  "$sync_script" work test-profile "$main_worktree"
grep -Fq "$expected_continue" "$output"
grep -Fq "$expected_abort" "$output"
grep -Fq "$expected_validate" "$output"
printf '%s\n' resolved >"$branch_worktree/shared"
git -C "$branch_worktree" add shared
GIT_EDITOR=true git -C "$branch_worktree" rebase --continue >/dev/null
run_sync --validate work test-profile "$main_worktree"
test "$(git -C "$branch_worktree" merge-base work origin/main)" = \
  "$remote_head"
grep -Fq \
  'Validated local main and work against origin/main.' "$output"

create_fixture validate-only
advance_origin first first
run_sync work test-profile "$main_worktree"
tracking_head=$(git -C "$main_worktree" rev-parse origin/main)
advance_origin second second
run_sync --validate work test-profile "$main_worktree"
test "$(git -C "$main_worktree" rev-parse origin/main)" = "$tracking_head"
grep -Fq 'Validation mode performed no fetch, rebase, push, or activation.' \
  "$output"

create_fixture stale-main
advance_origin upstream-only upstream
git -C "$main_worktree" fetch --quiet origin \
  refs/heads/main:refs/remotes/origin/main
if run_sync --validate work test-profile "$main_worktree"; then
  printf 'validated with stale local main\n' >&2
  exit 1
fi
grep -Fq 'local main does not match origin/main' "$output"
test ! -s "$log"

create_fixture formatter-output
export SYNC_TEST_FORMAT_UNTRACKED=true
if run_sync --validate work test-profile "$main_worktree"; then
  printf 'ignored formatter-created untracked content\n' >&2
  exit 1
fi
grep -Fq '?? formatter-output' "$output"
grep -Fq 'Validation failed during: Formatting work' "$output"
grep -Fq 'Validation mode performed no fetch or rebase.' "$output"

create_fixture check-failure
advance_origin upstream-only upstream
export SYNC_TEST_FAIL_CHECK=true
if run_sync work test-profile "$main_worktree"; then
  printf 'ignored a failed flake check\n' >&2
  exit 1
fi
remote_head=$(git -C "$origin" rev-parse refs/heads/main)
test "$(git -C "$main_worktree" rev-parse main)" = "$remote_head"
test "$(git -C "$branch_worktree" merge-base work origin/main)" = \
  "$remote_head"
grep -Fq 'Validation failed during: Validating flake checks' "$output"
grep -Fq \
  'Local main and work were synchronized before validation failed.' "$output"

create_fixture prunable
rm -rf -- "$branch_worktree"
if run_sync work test-profile "$main_worktree"; then
  printf 'accepted a prunable worktree registration\n' >&2
  exit 1
fi
grep -Fq 'worktree registration is prunable for branch: work' "$output"
