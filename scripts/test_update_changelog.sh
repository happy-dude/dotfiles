#!/usr/bin/env bash
# shellcheck disable=SC2329 # cleanup is invoked by the EXIT trap.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
TMPDIR_TEST="$(mktemp -d)"

export GIT_CONFIG_GLOBAL=/dev/null

cleanup() {
  rm -rf -- "$TMPDIR_TEST"
}
trap cleanup EXIT

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

commit_all() {
  local repo="$1"
  local message="$2"

  git -C "$repo" add -A
  git -C "$repo" \
    -c user.name='Update Test' \
    -c user.email='update-test@example.invalid' \
    commit -q -m "$message"
}

create_repo() {
  local repo="$1"

  mkdir -p -- "$repo"
  git -C "$repo" init -q
  printf 'initial\n' >"$repo/tracked"
  commit_all "$repo" initial
}

# shellcheck disable=SC1091 # Intentionally sources the repository script.
DOTFILES_UPDATE_SOURCE_ONLY=1 source "$REPO_DIR/scripts/update.sh"

VERBOSE=1
SHOW_CHANGES=0
resolve_output_options
[ "$SHOW_CHANGES" -eq 1 ] ||
  fail 'verbose output did not enable show-changes'
VERBOSE=0
SHOW_CHANGES=1
resolve_output_options
[ "$SHOW_CHANGES" -eq 1 ] ||
  fail 'quiet output disabled an explicit show-changes request'
[ "$VERBOSE" -eq 0 ] ||
  fail 'show-changes unexpectedly enabled verbose output'

generation_state="$TMPDIR_TEST/generation-state"
generation_old="$TMPDIR_TEST/generation-old"
generation_new="$TMPDIR_TEST/generation-new"
mkdir -p "$generation_state/nix/profiles" "$generation_old" "$generation_new"
ln -s "$generation_old" "$generation_state/nix/profiles/home-manager"
resolved_generation="$(
  XDG_STATE_HOME="$generation_state" current_home_manager_generation
)"
[ "$resolved_generation" = "$generation_old" ] ||
  fail 'current Home Manager generation was not resolved'

changelog_repo="$TMPDIR_TEST/changelog"
create_repo "$changelog_repo"
changelog_old="$(git -C "$changelog_repo" rev-parse HEAD)"
printf 'second\n' >>"$changelog_repo/tracked"
commit_all "$changelog_repo" 'second change'
changelog_new="$(git -C "$changelog_repo" rev-parse HEAD)"
changelog_output="$(
  cd "$changelog_repo"
  SECTION_RESULTS=()
  nix() {
    printf '%s\n' 'example-package: 1.0 → 2.0'
  }
  print_generation_changelog \
    "$generation_old" \
    "$generation_new" \
    "$changelog_old" \
    "$changelog_new" \
    1
  printf 'RESULT=%s\n' "${SECTION_RESULTS[0]}"
)"
grep -F "Previous: $generation_old" <<<"$changelog_output" >/dev/null ||
  fail 'generation changelog omitted the previous generation'
grep -F "Current:  $generation_new" <<<"$changelog_output" >/dev/null ||
  fail 'generation changelog omitted the current generation'
grep -F 'example-package: 1.0 → 2.0' <<<"$changelog_output" >/dev/null ||
  fail 'generation changelog omitted the closure delta'
grep -F 'Repository shortlog:' <<<"$changelog_output" >/dev/null ||
  fail 'generation changelog omitted the repository shortlog'
grep -F 'second change' <<<"$changelog_output" >/dev/null ||
  fail 'generation changelog omitted the new commit subject'
grep -F 'Committed Git changes:' <<<"$changelog_output" >/dev/null ||
  fail 'show-changes omitted the committed diff heading'
grep -F '+second' <<<"$changelog_output" >/dev/null ||
  fail 'show-changes omitted the committed diff'
grep -Fx 'RESULT=done' <<<"$changelog_output" >/dev/null ||
  fail 'changed generation was not recorded as done'

unchanged_output="$(
  cd "$changelog_repo"
  SECTION_RESULTS=()
  print_generation_changelog \
    "$generation_new" \
    "$generation_new" \
    "$changelog_new" \
    "$changelog_new" \
    0
  printf 'RESULT=%s\n' "${SECTION_RESULTS[0]}"
)"
grep -F 'Home Manager generation is unchanged:' \
  <<<"$unchanged_output" >/dev/null ||
  fail 'unchanged generation was not reported'
if grep -F 'Repository shortlog:' <<<"$unchanged_output" >/dev/null; then
  fail 'unchanged repository produced a shortlog'
fi
grep -Fx 'RESULT=skipped' <<<"$unchanged_output" >/dev/null ||
  fail 'unchanged changelog was not recorded as skipped'

printf 'staged-only\n' >>"$changelog_repo/tracked"
git -C "$changelog_repo" add tracked
staged_output="$(
  cd "$changelog_repo"
  SECTION_RESULTS=()
  print_generation_changelog \
    "$generation_new" \
    "$generation_new" \
    "$changelog_new" \
    "$changelog_new" \
    1
  printf 'RESULT=%s\n' "${SECTION_RESULTS[0]}"
)"
grep -F 'Staged Git changes:' <<<"$staged_output" >/dev/null ||
  fail 'show-changes omitted the staged diff heading'
grep -F '+staged-only' <<<"$staged_output" >/dev/null ||
  fail 'show-changes omitted the staged diff'
if grep -F 'Committed Git changes:' <<<"$staged_output" >/dev/null; then
  fail 'staged-only state was reported as a committed diff'
fi
grep -Fx 'RESULT=done' <<<"$staged_output" >/dev/null ||
  fail 'staged-only changelog was not recorded as done'
git -C "$changelog_repo" reset --hard -q HEAD

empty_output="$(
  cd "$changelog_repo"
  SECTION_RESULTS=()
  print_generation_changelog \
    "$generation_new" \
    "$generation_new" \
    "$changelog_new" \
    "$changelog_new" \
    1
  printf 'RESULT=%s\n' "${SECTION_RESULTS[0]}"
)"
grep -F 'No committed or staged Git changes to show.' \
  <<<"$empty_output" >/dev/null ||
  fail 'show-changes omitted the empty-state message'
grep -Fx 'RESULT=skipped' <<<"$empty_output" >/dev/null ||
  fail 'empty changelog was not recorded as skipped'

closure_error="$TMPDIR_TEST/closure-error"
failure_status=0
failure_output="$(
  (
    cd "$changelog_repo"
    SECTION_RESULTS=()
    nix() {
      return 1
    }
    print_generation_changelog \
      "$generation_old" \
      "$generation_new" \
      "$changelog_new" \
      "$changelog_new" \
      0
  ) 2>"$closure_error"
)" || failure_status=$?
[ "$failure_status" -eq 0 ] ||
  fail 'closure reporting failure changed the activation result'
grep -F 'Could not compare the Home Manager generation closures' \
  "$closure_error" >/dev/null ||
  fail 'closure reporting failure did not warn'
grep -F 'Previous:' <<<"$failure_output" >/dev/null ||
  fail 'closure reporting failure omitted generation context'

printf 'Updater generation changelog tests passed.\n'
