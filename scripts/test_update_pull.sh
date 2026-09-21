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
commit_all "$seed" 'tests: create pull base'
git -C "$seed" remote add origin "$origin"
git -C "$seed" push --quiet -u origin main
git clone --quiet "$origin" "$repo"
git -C "$repo" config user.name 'Update Test'
git -C "$repo" config user.email 'update-test@example.invalid'
git -C "$repo" config rebase.updateRefs true

printf 'first\n' >"$repo/first"
commit_all "$repo" 'tests: add first local change'
git -C "$repo" branch backup-first
first_backup=$(git -C "$repo" rev-parse backup-first)
printf 'second\n' >"$repo/second"
commit_all "$repo" 'tests: add second local change'
git -C "$repo" branch backup-tip
tip_backup=$(git -C "$repo" rev-parse backup-tip)
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
PATH="$fake_bin:$PATH" bash "$SCRIPT_DIR/update.sh" update \
  --skip-submodules --skip-nix-fmt --skip-nix-flake --skip-home-manager \
  "$repo" >"$TMPDIR_TEST/output" 2>&1

[[ $(git -C "$repo" merge-base HEAD origin/main) == "$upstream" ]] ||
  fail 'pull did not rebase onto the fetched upstream'
[[ $(git -C "$repo" rev-parse backup-first) == "$first_backup" ]] ||
  fail 'pull rewrote the backup of an intermediate commit'
[[ $(git -C "$repo" rev-parse backup-tip) == "$tip_backup" ]] ||
  fail 'pull rewrote the backup of the previous branch tip'
[[ $(git -C "$repo" config --bool rebase.updateRefs) == true ]] ||
  fail 'pull changed the configured policy for manual rebases'
[[ $(git -C "$origin" rev-parse main) == "$upstream" ]] ||
  fail 'pull changed the remote branch'
[[ $(<"$repo/tracked") == $'base\nuncommitted' ]] ||
  fail 'pull did not restore autostashed edits'
[[ $(<"$repo/first") == first && $(<"$repo/second") == second ]] ||
  fail 'pull lost local commits'
