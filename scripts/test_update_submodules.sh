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

topology_root="$TMPDIR_TEST/topology-root"
direct_remote="$TMPDIR_TEST/direct-remote"
leaf_remote="$TMPDIR_TEST/leaf-remote"
skipped_remote="$TMPDIR_TEST/skipped-remote"
new_remote="$TMPDIR_TEST/new-remote"

create_repo "$leaf_remote"
create_repo "$skipped_remote"
create_repo "$new_remote"
create_repo "$direct_remote"

git -C "$direct_remote" -c protocol.file.allow=always submodule add -q \
  "$leaf_remote" deps/leaf
git -C "$direct_remote" -c protocol.file.allow=always submodule add -q \
  "$skipped_remote" deps/skipped
git -C "$direct_remote" config -f .gitmodules \
  submodule.deps/skipped.update none
commit_all "$direct_remote" direct-one

direct_one="$(git -C "$direct_remote" rev-parse HEAD)"
leaf_one="$(git -C "$leaf_remote" rev-parse HEAD)"

create_repo "$topology_root"
git -C "$topology_root" -c protocol.file.allow=always submodule add -q \
  "$direct_remote" modules/direct
commit_all "$topology_root" root-one
git -C "$topology_root" -c protocol.file.allow=always \
  submodule update --init --recursive -q

[ "$(git -C "$topology_root/modules/direct" rev-parse HEAD)" = "$direct_one" ] ||
  fail 'direct submodule did not start at its recorded gitlink'
[ "$(git -C "$topology_root/modules/direct/deps/leaf" rev-parse HEAD)" = "$leaf_one" ] ||
  fail 'nested submodule did not start at its recorded gitlink'
[ ! -e "$topology_root/modules/direct/deps/skipped/.git" ] ||
  fail 'update=none submodule was initialized'

printf 'leaf two\n' >>"$leaf_remote/tracked"
commit_all "$leaf_remote" leaf-two
leaf_two="$(git -C "$leaf_remote" rev-parse HEAD)"

git -C "$direct_remote/deps/leaf" -c protocol.file.allow=always fetch -q origin
git -C "$direct_remote/deps/leaf" checkout -q --detach "$leaf_two"
git -C "$direct_remote" -c protocol.file.allow=always submodule add -q \
  "$new_remote" deps/new
commit_all "$direct_remote" direct-two

direct_two="$(git -C "$direct_remote" rev-parse HEAD)"
new_one="$(git -C "$new_remote" rev-parse HEAD)"

printf 'leaf three\n' >>"$leaf_remote/tracked"
commit_all "$leaf_remote" leaf-three
leaf_three="$(git -C "$leaf_remote" rev-parse HEAD)"

(
  cd "$topology_root"
  export GIT_ALLOW_PROTOCOL=file
  update_submodule_graph >/dev/null
)

[ "$(git -C "$topology_root/modules/direct" rev-parse HEAD)" = "$direct_two" ] ||
  fail 'direct submodule did not advance to its remote tip'
[ "$(git -C "$topology_root/modules/direct/deps/leaf" rev-parse HEAD)" = "$leaf_two" ] ||
  fail 'nested submodule did not follow its direct parent gitlink'
[ "$(git -C "$topology_root/modules/direct/deps/leaf" rev-parse HEAD)" != "$leaf_three" ] ||
  fail 'nested submodule incorrectly advanced to its own remote tip'
[ "$(git -C "$topology_root/modules/direct/deps/new" rev-parse HEAD)" = "$new_one" ] ||
  fail 'new nested submodule was not initialized'
[ ! -e "$topology_root/modules/direct/deps/skipped/.git" ] ||
  fail 'update=none submodule was initialized during reconciliation'
[[ "$(git -C "$topology_root/modules/direct" \
  submodule status -- deps/skipped)" == -* ]] ||
  fail 'update=none submodule was not reported as uninitialized'
[ -z "$(git -C "$topology_root/modules/direct" \
  status --porcelain --untracked-files=all)" ] ||
  fail 'direct submodule remained internally dirty after reconciliation'
[ "$(git -C "$topology_root" status --porcelain)" = ' M modules/direct' ] ||
  fail 'root did not retain exactly the direct submodule update'

printf 'Updater submodule safety tests passed.\n'
