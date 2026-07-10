#!/usr/bin/env bash
# shellcheck disable=SC2329 # cleanup is invoked by the EXIT trap.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
TMPDIR_TEST="$(mktemp -d)"

export GIT_CONFIG_GLOBAL=/dev/null
export XDG_CONFIG_HOME="$TMPDIR_TEST/xdg"

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

source_repo="$TMPDIR_TEST/source"
parent_repo="$TMPDIR_TEST/parent"
child_repo="$parent_repo/modules/child"

create_repo "$source_repo"
create_repo "$parent_repo"
git -C "$parent_repo" -c protocol.file.allow=always submodule add -q \
  "$source_repo" modules/child
commit_all "$parent_repo" submodule

printf 'changed\n' >>"$child_repo/tracked"
(
  cd "$parent_repo"
  STASHED_SUBMODULES=()
  stash_dirty_submodules >/dev/null
  [ "${#STASHED_SUBMODULES[@]}" -eq 1 ] || fail 'valid change was not recorded'
  [ -z "$(git -C modules/child status --porcelain --untracked-files=normal)" ] ||
    fail 'valid auto-stash did not clean the submodule'
)

valid_stash="$(git -C "$child_repo" rev-parse 'stash@{0}')"
stash_contains_changes "$child_repo" "$valid_stash" ||
  fail 'tracked change was classified as an empty stash'
git -C "$child_repo" stash clear

embedded_repo="$child_repo/nested"
create_repo "$embedded_repo"
mapfile -d '' -t embedded < <(find_untracked_embedded_repositories "$child_repo")
[ "${#embedded[@]}" -eq 1 ] || fail 'embedded repository was not detected'
[ "${embedded[0]}" = nested ] || fail 'unexpected embedded repository path'

stash_count_before="$(git -C "$child_repo" stash list --format='%H' | wc -l)"
if (
  cd "$parent_repo"
  STASHED_SUBMODULES=()
  stash_dirty_submodules >/dev/null 2>&1
); then
  fail 'auto-stash accepted an embedded repository'
fi
stash_count_after="$(git -C "$child_repo" stash list --format='%H' | wc -l)"
[ "$stash_count_before" -eq "$stash_count_after" ] ||
  fail 'embedded repository created an empty stash'

empty_tree="$(git -C "$child_repo" rev-parse 'HEAD^{tree}')"
empty_stash="$(
  printf 'synthetic empty stash\n' |
    git -C "$child_repo" \
      -c user.name='Update Test' \
      -c user.email='update-test@example.invalid' \
      commit-tree "$empty_tree" -p HEAD
)"
if stash_contains_changes "$child_repo" "$empty_stash"; then
  fail 'empty stash was classified as containing changes'
fi

help_repo="$TMPDIR_TEST/help"
create_repo "$help_repo"
mkdir -p -- "$help_repo/doc"
printf 'original\n' >"$help_repo/doc/tags"
commit_all "$help_repo" help-tags
printf 'generated\n' >>"$help_repo/doc/tags"

# shellcheck disable=SC2034 # Read by the sourced restore helper.
CLEAN_VIM_HELP_TAGS=("$help_repo")
RESTORED_VIM_HELP_TAGS=()
restore_generated_vim_help_tags
git -C "$help_repo" diff --quiet -- doc/tags ||
  fail 'generated help tags were not restored'
[ "${#RESTORED_VIM_HELP_TAGS[@]}" -eq 1 ] ||
  fail 'restored help tags were not reported'

printf 'Updater submodule safety tests passed.\n'
