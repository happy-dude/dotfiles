# shellcheck shell=bash
# Sourced by the test suites; it has no shebang of its own.

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

TEST_HELPERS_DIRECTORY=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

# Clear every variable that points Git at a repository. An inherited GIT_DIR
# outranks both the working directory and -C. git rebase --exec exports it in
# a linked worktree, as hooks do, and with it set every fixture command acts
# on the caller's repository: a fixture git init there sets core.bare in the
# caller's shared config.
isolate_git_environment() {
  local variables

  variables=$(git rev-parse --local-env-vars) || return 1
  # shellcheck disable=SC2086 # One variable name per word.
  unset $variables
}

test_config_digest() {
  sha256sum <"$1" 2>/dev/null || printf 'unreadable\n'
}

# Put a git stub first on PATH that refuses any Git command aimed outside
# TMPDIR_TEST, so a fixture path that resolves somewhere else stops the suite
# instead of changing that place. Children inherit it through PATH.
install_git_guard() {
  local guard_directory="$TMPDIR_TEST/.git-guard"
  local real_git
  local shell

  real_git=$(type -P git) || return 1
  shell=$(type -P bash) || return 1
  mkdir -- "$guard_directory" || return 1
  {
    printf '#!%s\n' "$shell"
    printf 'exec %q %q %q %q "$@"\n' "$shell" \
      "$TEST_HELPERS_DIRECTORY/test-git-guard.sh" "$TMPDIR_TEST" "$real_git"
  } >"$guard_directory/git" || return 1
  chmod 0755 "$guard_directory/git" || return 1
  export TEST_GIT_GUARD_ROOT="$TMPDIR_TEST"
  export PATH="$guard_directory:$PATH"
}

# Remove TMPDIR_TEST, then fail the suite if it changed the configuration of
# the repository it was started from, whatever its assertions concluded.
# shellcheck disable=SC2329 # Invoked by the EXIT trap test_setup installs.
cleanup_test_tmpdir() {
  rm -rf -- "$TMPDIR_TEST"
  if [[ -n ${TEST_INVOKING_CONFIG:-} ]] &&
    [[ $(test_config_digest "$TEST_INVOKING_CONFIG") != "$TEST_INVOKING_CONFIG_DIGEST" ]]; then
    printf 'FAIL: the suite changed the configuration of the repository it ran from: %s\n' \
      "$TEST_INVOKING_CONFIG" >&2
    exit 1
  fi
}

# Create TMPDIR_TEST, remove it on exit, and keep fixture commits
# independent of the machine's global git configuration. Owns the suite's
# EXIT trap: a suite that installs its own trap must chain this cleanup.
#
# Fixtures are Git repositories, so the suite is also confined to
# TMPDIR_TEST. In order:
#
# - The shared config of the repository the suite was started from, if any,
#   is recorded, and cleanup_test_tmpdir fails the suite if it changed.
# - Inherited Git location variables are cleared.
# - GIT_CEILING_DIRECTORIES stops repository discovery below the parent of
#   TMPDIR_TEST, so an empty or never-initialised fixture path is "not a
#   repository" even when TMPDIR is inside a checkout.
# - The guard refuses Git commands aimed outside TMPDIR_TEST.
# - The working directory moves into TMPDIR_TEST, where a Git command with no
#   -C, or an empty one, finds nothing. Paths a suite needs afterwards have to
#   be absolute.
#
# A suite started by another suite, inside that suite's TMPDIR_TEST, keeps
# the outer guard, whose directory already contains its own.
test_setup() {
  local common_directory
  local ceiling
  local nested=false

  if [[ -n ${TEST_GIT_GUARD_ROOT:-} &&
    $(type -P git) == "$TEST_GIT_GUARD_ROOT/.git-guard/git" ]]; then
    nested=true
  fi
  TEST_INVOKING_CONFIG=
  TEST_INVOKING_CONFIG_DIGEST=
  if [[ $nested == false ]] && common_directory=$(
    git rev-parse --path-format=absolute --git-common-dir 2>/dev/null
  ); then
    TEST_INVOKING_CONFIG="$common_directory/config"
    TEST_INVOKING_CONFIG_DIGEST=$(test_config_digest "$TEST_INVOKING_CONFIG")
  fi
  isolate_git_environment || fail 'could not clear the Git environment'

  TMPDIR_TEST="$(mktemp -d)"
  # The guard compares physical paths.
  TMPDIR_TEST=$(cd -- "$TMPDIR_TEST" && pwd -P)
  [[ -n $TMPDIR_TEST && -d $TMPDIR_TEST ]] ||
    fail 'could not create the test directory'
  trap cleanup_test_tmpdir EXIT
  # Scripts under test create temporary files and directories with mktemp;
  # keep those inside the guarded directory too.
  mkdir -- "$TMPDIR_TEST/.tmp" || fail 'could not create the test directory'
  export TMPDIR="$TMPDIR_TEST/.tmp"
  export GIT_CONFIG_GLOBAL=/dev/null
  export GIT_CONFIG_SYSTEM=/dev/null
  export XDG_CONFIG_HOME="$TMPDIR_TEST/xdg"
  # An empty entry has a meaning of its own in this list, so the root
  # directory is spelled out.
  ceiling=${TMPDIR_TEST%/*}
  export GIT_CEILING_DIRECTORIES="${ceiling:-/}${GIT_CEILING_DIRECTORIES:+:$GIT_CEILING_DIRECTORIES}"
  if [[ $nested == false ]]; then
    install_git_guard || fail 'could not install the Git guard'
  fi
  cd -- "$TMPDIR_TEST" || fail 'could not enter the test directory'
}

# Commit everything in a fixture repository under a fixed identity so the
# fixture is reproducible without user git configuration.
commit_all() {
  local repo="$1"
  local message="$2"

  git -C "$repo" add -A
  git -C "$repo" \
    -c user.name='Update Test' \
    -c user.email='update-test@example.invalid' \
    commit -q -m "$message"
}

# Create a fixture repository with one tracked commit.
create_repo() {
  local repo="$1"

  mkdir -p -- "$repo"
  git -C "$repo" init -q --initial-branch=main
  printf 'initial\n' >"$repo/tracked"
  commit_all "$repo" initial
}
