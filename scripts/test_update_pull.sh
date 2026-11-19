#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/test-helpers.sh
source "$SCRIPT_DIR/lib/test-helpers.sh"
test_setup

origin="$TMPDIR_TEST/origin.git"
seed="$TMPDIR_TEST/seed"
repo="$TMPDIR_TEST/checkout"
fake_bin="$TMPDIR_TEST/bin"
mkdir "$seed" "$fake_bin"
git init --quiet --bare --initial-branch=main "$origin"
git -C "$seed" init --quiet --initial-branch=main
printf 'base\n' >"$seed/tracked"
printf 'initial lock\n' >"$seed/flake.lock"
commit_all "$seed" 'tests: create pull base'
git -C "$seed" remote add origin "$origin"
git -C "$seed" push --quiet -u origin main
git clone --quiet "$origin" "$repo"
git -C "$repo" config user.name 'Update Test'
git -C "$repo" config user.email 'update-test@example.invalid'
git -C "$repo" config rebase.updateRefs true

printf 'label = "a b";\n' >"$repo/first"
commit_all "$repo" 'tests: add first local change'
git -C "$repo" branch backup-first
first_backup=$(git -C "$repo" rev-parse backup-first)
printf 'second\n' >"$repo/second"
commit_all "$repo" 'tests: add second local change'
git -C "$repo" branch backup-tip
tip_backup=$(git -C "$repo" rev-parse backup-tip)
printf 'label = "ab";\n' >"$seed/first"
commit_all "$seed" 'tests: apply the whitespace-different change'
printf 'upstream\n' >"$seed/upstream"
commit_all "$seed" 'tests: advance upstream'
git -C "$seed" push --quiet origin main
upstream=$(git -C "$seed" rev-parse HEAD)
printf 'uncommitted\n' >>"$repo/tracked"

# Only Nix validation is substituted. Git acts on real disposable repositories,
# and the updater retains every production guard and its supported skip flags.
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/nix"
cat >>"$fake_bin/nix" <<'EOF'
[[ $1 == flake && $2 == check ]] || exit 91
EOF
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/home-manager"
cat >>"$fake_bin/home-manager" <<'EOF'
[[ $1 == build ]] || exit 92
EOF
chmod 0755 "$fake_bin/nix" "$fake_bin/home-manager"
# If pull autostashes first, the staged blob can already be gone when the
# post-validation lock commit checks the index.
printf 'staged lock\n' >"$repo/flake.lock"
git -C "$repo" add flake.lock
staged_lock=$(git -C "$repo" rev-parse :flake.lock)
printf 'working lock\n' >"$repo/flake.lock"
tracking_before=$(git -C "$repo" rev-parse origin/main)
if PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-submodules --skip-nix-fmt --skip-nix-flake --skip-home-manager \
  "$repo" >"$TMPDIR_TEST/output" 2>&1; then
  fail 'pull autostashed a separately staged lock'
fi
grep -q 'not pulling' "$TMPDIR_TEST/output"
test "$(git -C "$repo" rev-parse HEAD)" = "$tip_backup"
test "$(git -C "$repo" rev-parse origin/main)" = "$tracking_before"
test "$(git -C "$repo" rev-parse :flake.lock)" = "$staged_lock"
test "$(<"$repo/flake.lock")" = 'working lock'
if PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-pull --skip-submodules --skip-home-manager \
  "$repo" >"$TMPDIR_TEST/output" 2>&1; then
  fail 'a mutable flake update accepted a separately staged lock'
fi
grep -q 'not rewriting a separately staged' "$TMPDIR_TEST/output"
test "$(git -C "$repo" rev-parse :flake.lock)" = "$staged_lock"
test "$(<"$repo/flake.lock")" = 'working lock'
git -C "$repo" restore --source=HEAD --staged --worktree flake.lock

if PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-submodules --skip-nix-fmt --skip-nix-flake --skip-home-manager \
  "$repo" >"$TMPDIR_TEST/output" 2>&1; then
  fail 'updater rebased a divergent local branch'
fi
grep -q 'local history diverges' "$TMPDIR_TEST/output"
git -C "$repo" cherry origin/main main | grep -Fx -- "- $first_backup" >/dev/null ||
  fail 'fixture did not exercise a whitespace patch-ID collision'
test "$(git -C "$repo" rev-parse HEAD)" = "$tip_backup"
test "$(<"$repo/first")" = 'label = "a b";'
[[ $(git -C "$repo" rev-parse backup-first) == "$first_backup" ]] ||
  fail 'pull rewrote the backup of an intermediate commit'
[[ $(git -C "$repo" rev-parse backup-tip) == "$tip_backup" ]] ||
  fail 'pull rewrote the backup of the previous branch tip'
[[ $(git -C "$repo" config --bool rebase.updateRefs) == true ]] ||
  fail 'pull changed the configured policy for manual rebases'
[[ $(git -C "$origin" rev-parse main) == "$upstream" ]] ||
  fail 'pull changed the remote branch'
[[ $(<"$repo/tracked") == $'base\nuncommitted' ]] ||
  fail 'divergence refusal changed a dirty file'

# A fast-forward still preserves an unrelated dirty edit.
git -C "$repo" switch -qc ff-fixture "$tracking_before"
git -C "$repo" branch -u origin/main ff-fixture
PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-submodules --skip-nix-fmt --skip-nix-flake --skip-home-manager \
  "$repo" >"$TMPDIR_TEST/output" 2>&1
test "$(git -C "$repo" rev-parse HEAD)" = "$upstream"
test "$(<"$repo/tracked")" = $'base\nuncommitted'

# Git reports success when its autostash cannot be restored and leaves the
# conflict in the index. Validation and the lock commit must not run on that.
printf 'conflicting upstream\n' >"$seed/tracked"
commit_all "$seed" 'tests: change the autostashed file upstream'
git -C "$seed" push --quiet origin main
if PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-submodules --skip-nix-fmt --skip-nix-flake --skip-home-manager \
  "$repo" >"$TMPDIR_TEST/output" 2>&1; then
  fail 'update continued after the autostash could not be restored'
fi
grep -q 'autostash could not be restored' "$TMPDIR_TEST/output" ||
  fail 'autostash conflict was not reported'
grep -q 'Validating flake checks' "$TMPDIR_TEST/output" &&
  fail 'update validated a checkout with unresolved conflicts'
[[ -n $(git -C "$repo" ls-files --unmerged) ]] ||
  fail 'fixture did not produce an autostash conflict'

# A subdirectory path still selects the whole repository.
nested="$TMPDIR_TEST/nested"
create_repo "$nested"
mkdir "$nested/subdirectory"
printf 'lock-v1\n' >"$nested/flake.lock"
printf 'kept\n' >"$nested/subdirectory/file"
commit_all "$nested" 'tests: create a nested checkout'
git -C "$nested" config user.name 'Update Test'
git -C "$nested" config user.email 'update-test@example.invalid'
printf 'lock-v2\n' >"$nested/flake.lock"
PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-pull --skip-nix-fmt --skip-nix-flake --skip-home-manager \
  "$nested/subdirectory" >"$TMPDIR_TEST/output" 2>&1 ||
  fail 'update from a subdirectory failed'
[[ $(git -C "$nested" log -1 --format=%s) == 'nix: update flake.lock' ]] ||
  fail 'update from a subdirectory did not commit the root flake.lock'
