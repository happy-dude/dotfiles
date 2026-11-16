#!/usr/bin/env bash

# The suites create and rewrite Git repositories, so a suite whose Git
# commands reach the repository it was started from can damage it: a fixture
# git init run under an exported GIT_DIR sets core.bare in the caller's shared
# config, and the fixture identity and a push to origin follow. This suite
# plays that caller. It starts the portable suites from inside a decoy
# checkout, with the variable git rebase --exec exports in a linked worktree,
# and requires the decoy to come back byte for byte. It then checks each
# barrier test_setup puts up on its own.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
# What a caller outside any suite has, before test_setup changes it.
caller_path=$PATH
caller_tmpdir=${TMPDIR:-}
# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/test-helpers.sh
source "$SCRIPT_DIR/lib/test-helpers.sh"
test_setup

# A decoy checkout with a linked worktree and a remote, as the real one has.
decoy="$TMPDIR_TEST/decoy"
decoy_linked="$TMPDIR_TEST/decoy-linked"
decoy_origin="$TMPDIR_TEST/decoy-origin.git"
decoy_config="$decoy/.git/config"
git init -q --bare --initial-branch=main "$decoy_origin"
create_repo "$decoy"
git -C "$decoy" remote add origin "$decoy_origin"
git -C "$decoy" push -q -u origin main
git -C "$decoy" config extensions.worktreeConfig true
git -C "$decoy" worktree add -q -b side "$decoy_linked"
cp -- "$decoy_config" "$TMPDIR_TEST/decoy-config.before"

decoy_state() {
  git -C "$decoy" for-each-ref
  git -C "$decoy_origin" for-each-ref
  git -C "$decoy" status --porcelain=v1 --untracked-files=all
  git -C "$decoy_linked" status --porcelain=v1 --untracked-files=all
}
decoy_state >"$TMPDIR_TEST/decoy-state.before"

# Run a command the way a caller outside any suite would: without this
# suite's guard and ceiling, from inside the decoy's linked worktree, and with
# GIT_DIR exported the way git rebase --exec exports it there. Extra NAME=value
# words before the command are passed to env.
run_as_caller() {
  local -a environment=(
    -u TEST_GIT_GUARD_ROOT
    -u GIT_CEILING_DIRECTORIES
    -u TMPDIR
    "PATH=$caller_path"
    "GIT_DIR=$decoy/.git/worktrees/decoy-linked"
  )

  # env reads its options before the first assignment, so the caller's
  # TMPDIR, when it has one, is put back after being cleared.
  if [[ -n $caller_tmpdir ]]; then
    environment+=("TMPDIR=$caller_tmpdir")
  fi
  (
    cd -- "$decoy_linked"
    env "${environment[@]}" "$@"
  )
}

assert_decoy_untouched() {
  local description=$1

  cmp -s -- "$decoy_config" "$TMPDIR_TEST/decoy-config.before" ||
    fail "$description: the caller's shared Git config changed"
  [[ $(decoy_state) == "$(<"$TMPDIR_TEST/decoy-state.before")" ]] ||
    fail "$description: the caller's refs, remote, or worktrees changed"
}

# The three portable suites. Each must pass, and leave the decoy alone, when
# started the way that would otherwise rewrite the caller's repository.
for suite in portable_series portable_export apply_portable_series; do
  status=0
  run_as_caller bash "$SCRIPT_DIR/test_$suite.sh" \
    >"$TMPDIR_TEST/$suite.log" 2>&1 || status=$?
  assert_decoy_untouched "test_$suite.sh under an exported GIT_DIR"
  ((status == 0)) ||
    fail "test_$suite.sh failed under an exported GIT_DIR: $(tail -n 5 "$TMPDIR_TEST/$suite.log")"
done

# test_setup records the config of the repository a suite starts from and
# fails the suite at exit if it changed, even when every assertion passed.
# The probe changes it behind the guard's back, with the Git the caller has.
probe="$TMPDIR_TEST/probe.sh"
cat >"$probe" <<'EOF'
set -euo pipefail
caller_git=$(type -P git)
source "$1/lib/test-helpers.sh"
test_setup
printf 'recorded=%s\n' "$TEST_INVOKING_CONFIG"
case $2 in
change) "$caller_git" config --file "$TEST_INVOKING_CONFIG" probe.changed true ;;
keep) ;;
esac
EOF
output=$(run_as_caller bash "$probe" "$SCRIPT_DIR" keep 2>&1) ||
  fail "a suite that changed nothing failed: $output"
[[ $output == *"recorded=$decoy_config"* ]] ||
  fail "test_setup did not record the caller's shared config: $output"
status=0
output=$(run_as_caller bash "$probe" "$SCRIPT_DIR" change 2>&1) || status=$?
((status != 0)) ||
  fail 'a suite that changed the caller configuration still passed'
[[ $output == *"changed the configuration of the repository"* ]] ||
  fail "the changed caller configuration was not reported: $output"
cp -- "$TMPDIR_TEST/decoy-config.before" "$decoy_config"
assert_decoy_untouched 'restoring the decoy'

# A fixture path that is empty, or that was never initialised, must fail as
# "not a repository" instead of finding the checkout around it. This probe's
# temporary directory sits inside the decoy's worktree, so only the ceiling
# keeps discovery from climbing into the decoy.
mkdir "$decoy_linked/enclosed-tmp"
cat >"$probe" <<'EOF'
set -euo pipefail
source "$1/lib/test-helpers.sh"
test_setup
empty=
mkdir "$TMPDIR_TEST/never-initialised"
for directory in "$empty" "$TMPDIR_TEST/never-initialised"; do
  if found=$(git -C "$directory" rev-parse --absolute-git-dir 2>/dev/null); then
    printf 'found=%s\n' "$found"
    exit 1
  fi
done
if found=$(git rev-parse --absolute-git-dir 2>/dev/null); then
  printf 'found=%s\n' "$found"
  exit 1
fi
git init -q "$TMPDIR_TEST/fixture"
found=$(git -C "$TMPDIR_TEST/fixture" rev-parse --absolute-git-dir)
[[ $found == "$TMPDIR_TEST/fixture/.git" ]]
EOF
output=$(
  run_as_caller "TMPDIR=$decoy_linked/enclosed-tmp" \
    bash "$probe" "$SCRIPT_DIR" 2>&1
) || fail "a fixture path found a repository outside the suite: $output"
rm -rf -- "$decoy_linked/enclosed-tmp"
assert_decoy_untouched 'the enclosed temporary directory'

# A Git command aimed outside the suite's temporary directory is refused
# before Git runs, however the outside path is spelled. The target is a
# repository outside this suite's own directory, made with the caller's Git.
real_git=$(PATH=$caller_path type -P git)
outside=$(TMPDIR=${caller_tmpdir:-/tmp} mktemp -d)
outside=$(cd -- "$outside" && pwd -P)
trap 'rm -rf -- "$outside"; cleanup_test_tmpdir' EXIT
"$real_git" init -q --initial-branch=main "$outside"
cp -- "$outside/.git/config" "$TMPDIR_TEST/outside-config.before"
mkdir "$TMPDIR_TEST/inside"

assert_refused() {
  local description=$1
  local status=0
  local output
  shift

  output=$("$@" 2>&1) || status=$?
  ((status == 97)) ||
    fail "$description: expected the guard's status 97, got $status: $output"
  [[ $output == *"test guard: refusing"* ]] ||
    fail "$description: unexpected message: $output"
  cmp -s -- "$outside/.git/config" "$TMPDIR_TEST/outside-config.before" ||
    fail "$description: the outside repository changed"
}

assert_refused 'absolute -C' git -C "$outside" config user.name Fixture
assert_refused 'relative -C that climbs out' \
  git -C "$TMPDIR_TEST/inside" -C ../.. config user.name Fixture
assert_refused '--git-dir=' \
  git --git-dir="$outside/.git" config user.name Fixture
assert_refused '--git-dir with a separate value' \
  git --git-dir "$outside/.git" config user.name Fixture
assert_refused 'GIT_DIR in the environment' \
  env "GIT_DIR=$outside/.git" git config user.name Fixture
# shellcheck disable=SC2016 # The inner shell expands its own argument.
assert_refused 'a working directory outside' \
  bash -c 'cd -- "$1" && git config user.name Fixture' _ "$outside"
# The same command aimed inside the temporary directory runs.
git init -q "$TMPDIR_TEST/inside"
git -C "$TMPDIR_TEST/inside" config user.name Fixture
[[ $(git -C "$TMPDIR_TEST/inside" config --get user.name) == Fixture ]] ||
  fail 'the guard blocked a Git command inside the temporary directory'

printf 'git-isolation: the suites leave the invoking repository alone\n'
