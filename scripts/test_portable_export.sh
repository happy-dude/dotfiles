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
worktree="$TMPDIR_TEST/series"
output="$TMPDIR_TEST/output"
fake_bin="$TMPDIR_TEST/bin"
mkdir -p "$seed/scripts" "$output" "$fake_bin"
cp -- "$SCRIPT_DIR/portable-series.sh" "$SCRIPT_DIR/apply-portable-series.sh" \
  "$SCRIPT_DIR/lint_commit_message.py" "$seed/scripts/"
cp -R -- "$SCRIPT_DIR/lib" "$seed/scripts/"
chmod -R u+w "$seed/scripts"
git init --quiet --bare --initial-branch=main "$origin"
git -C "$seed" init --quiet --initial-branch=main
commit_all "$seed" 'tests: create portable export base'
git -C "$seed" remote add origin "$origin"
git -C "$seed" push --quiet -u origin main
git clone --quiet "$origin" "$repo"

run_portable() {
  (
    cd -- "$repo"
    bash scripts/portable-series.sh "$@"
  ) >"$TMPDIR_TEST/export.log" 2>&1
}

run_portable start probe "$worktree"
printf 'portable\n' >"$worktree/feature"
git -C "$worktree" add feature
git -C "$worktree" commit --quiet -m 'tests: add a portable feature'

# Substitute only the Nix boundary; Git, linter, patching, and artifact
# publication are real. Partial eval output must not hide a failed evaluation.
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/nix"
cat >>"$fake_bin/nix" <<'EOF'
set -euo pipefail
case $1 in
fmt)
  if [[ ${PORTABLE_TEST_FMT_CREATES:-} ]]; then
    printf 'generated\n' >"$PORTABLE_TEST_FMT_CREATES"
  fi
  ;;
flake) ;;
eval)
  printf 'first\nsecond\n'
  exit "${PORTABLE_TEST_EVAL_STATUS:-0}"
  ;;
*) exit 91 ;;
esac
EOF
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/home-manager"
cat >>"$fake_bin/home-manager" <<'EOF'
set -euo pipefail
[[ $1 == build && $2 == --flake ]] || exit 92
if [[ $3 == '.#second' && ${PORTABLE_TEST_BUILD_FAIL:-false} == true ]]; then
  exit 23
fi
EOF
chmod 0755 "$fake_bin/nix" "$fake_bin/home-manager"
export PATH="$fake_bin:$PATH"

run_portable export probe "$output"
(
  cd -- "$output"
  sha256sum --check dotfiles-probe.sha256 dotfiles-probe.tar.gz.sha256
)
before=$(sha256sum "$output"/*)

export PORTABLE_TEST_EVAL_STATUS=23
if run_portable export probe "$output"; then
  fail 'export published artifacts after profile enumeration failed'
fi
[[ $(sha256sum "$output"/*) == "$before" ]] ||
  fail 'failed profile enumeration replaced published artifacts'
shopt -s nullglob
staging=("$output"/.dotfiles-probe.*)
((${#staging[@]} == 0)) || fail 'failed export left a staging directory'
unset PORTABLE_TEST_EVAL_STATUS

# Building only the first profile would miss this failure.
export PORTABLE_TEST_BUILD_FAIL=true
if run_portable export probe "$output"; then
  fail 'export published artifacts after a declared profile failed to build'
fi
[[ $(sha256sum "$output"/*) == "$before" ]] ||
  fail 'failed profile build replaced published artifacts'
unset PORTABLE_TEST_BUILD_FAIL

# A file the formatter creates is drift too.
export PORTABLE_TEST_FMT_CREATES="$worktree/formatter-output"
if run_portable export probe "$output"; then
  fail 'export published artifacts after the formatter created a file'
fi
grep -q 'formatter changed the portable worktree' "$TMPDIR_TEST/export.log" ||
  fail 'untracked formatter output was not reported'
[[ $(sha256sum "$output"/*) == "$before" ]] ||
  fail 'formatter drift replaced published artifacts'
unset PORTABLE_TEST_FMT_CREATES
rm -f -- "$worktree/formatter-output"

# Apply needs one message per commit, and git am refuses an empty patch.
git -C "$repo" config format.coverLetter true
run_portable export probe "$output" ||
  fail 'export failed with format.coverLetter configured'
[[ $(grep -c '^From [0-9a-f]\{40\} ' "$output/dotfiles-probe.patch") == 1 ]] ||
  fail 'export published a cover letter the apply count rejects'
git -C "$repo" config --unset format.coverLetter
before=$(sha256sum "$output"/*)
git -C "$worktree" commit --quiet --allow-empty -m 'tests: an empty commit'
if run_portable export probe "$output"; then
  fail 'export published an empty commit git am refuses'
fi
grep -q 'empty commit' "$TMPDIR_TEST/export.log" ||
  fail 'empty commit was not reported'
[[ $(sha256sum "$output"/*) == "$before" ]] ||
  fail 'empty-commit rejection replaced published artifacts'
git -C "$worktree" reset --quiet --hard HEAD~1

# A narrow fetch mapping must not leave export validating against a stale base.
git -C "$repo" config --unset-all remote.origin.fetch
git -C "$repo" config --add remote.origin.fetch \
  '+refs/heads/unrelated:refs/remotes/origin/unrelated'
printf 'new upstream\n' >"$seed/upstream"
commit_all "$seed" 'tests: advance upstream after starting a series'
git -C "$seed" push --quiet origin main
upstream=$(git -C "$seed" rev-parse HEAD)
if run_portable export probe "$output"; then
  fail 'export accepted a series based on stale origin/main'
fi
[[ $(git -C "$repo" rev-parse origin/main) == "$upstream" ]] ||
  fail 'export did not refresh origin/main with a narrow fetch mapping'
[[ $(sha256sum "$output"/*) == "$before" ]] ||
  fail 'stale-base rejection replaced published artifacts'
