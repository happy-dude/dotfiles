# shellcheck shell=bash
# Sourced by the test suites; it has no shebang of its own.

# Fail the suite with a message.
fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

# shellcheck disable=SC2329 # Invoked by the EXIT trap test_setup installs.
cleanup_test_tmpdir() {
  rm -rf -- "$TMPDIR_TEST"
}

# Create TMPDIR_TEST, remove it on exit, and keep fixture commits
# independent of the machine's global git configuration. Owns the suite's
# EXIT trap: a suite that installs its own trap must chain this cleanup.
test_setup() {
  TMPDIR_TEST="$(mktemp -d)"
  export GIT_CONFIG_GLOBAL=/dev/null
  trap cleanup_test_tmpdir EXIT
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
  git -C "$repo" init -q
  printf 'initial\n' >"$repo/tracked"
  commit_all "$repo" initial
}
