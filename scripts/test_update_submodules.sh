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

printf 'staged\n' >>"$child_repo/tracked"
git -C "$child_repo" add tracked
staged_head="$(git -C "$child_repo" rev-parse HEAD)"
git -C "$child_repo" stash push -u -m staged-only-test >/dev/null
staged_stash="$(git -C "$child_repo" rev-parse 'stash@{0}')"

stash_has_expected_graph "$child_repo" "$staged_stash" "$staged_head" ||
  fail 'standard staged-only stash graph was rejected'
if git -C "$child_repo" diff --quiet \
  "${staged_stash}^1" "${staged_stash}^2" --; then
  fail 'staged-only fixture did not change the stash index parent'
fi
git -C "$child_repo" diff --quiet "${staged_stash}^2" "$staged_stash" -- ||
  fail 'staged-only fixture unexpectedly changed the stash worktree parent'
stash_contains_changes "$child_repo" "$staged_stash" ||
  fail 'staged-only change was classified as an empty stash'
git -C "$child_repo" stash clear

printf 'changed\n' >>"$child_repo/tracked"
valid_head="$(git -C "$child_repo" rev-parse HEAD)"
(
  cd "$parent_repo"
  collect_dirty_submodules
  [ "${#DIRTY_SUBMODULES[@]}" -eq 1 ] ||
    fail 'valid dirty submodule was not collected'
  [ "${DIRTY_SUBMODULES[0]}" = modules/child ] ||
    fail 'unexpected dirty submodule path'

  STASHED_SUBMODULES=()
  stash_dirty_submodules "${DIRTY_SUBMODULES[@]}" >/dev/null
  [ "${#STASHED_SUBMODULES[@]}" -eq 1 ] ||
    fail 'valid change was not recorded'
  if is_submodule_dirty modules/child; then
    fail 'valid auto-stash did not clean the submodule'
  fi
)

valid_stash="$(git -C "$child_repo" rev-parse 'stash@{0}')"
stash_has_expected_graph "$child_repo" "$valid_stash" "$valid_head" ||
  fail 'valid auto-stash graph was rejected'
stash_contains_changes "$child_repo" "$valid_stash" ||
  fail 'tracked change was classified as an empty stash'
git -C "$child_repo" stash clear

printf 'retained\n' >>"$child_repo/tracked"
retention_log="$TMPDIR_TEST/retention.log"
stash_count_before="$(git -C "$child_repo" stash list --format='%H' | wc -l)"
if (
  stash_contains_changes() {
    return 1
  }
  STASHED_SUBMODULES=()
  stash_dirty_submodules "$child_repo"
) >"$TMPDIR_TEST/retention.out" 2>"$retention_log"; then
  fail 'auto-stash accepted a forced empty-payload classification'
fi
stash_count_after="$(git -C "$child_repo" stash list --format='%H' | wc -l)"
[ "$stash_count_after" -eq $((stash_count_before + 1)) ] ||
  fail 'anomalous auto-stash was not retained'
retained_stash="$(git -C "$child_repo" rev-parse refs/stash)"
grep -F "retained auto-stash for review: $retained_stash" "$retention_log" \
  >/dev/null || fail 'retained auto-stash OID was not reported'
stash_contains_changes "$child_repo" "$retained_stash" ||
  fail 'retained auto-stash lost its payload'
git -C "$child_repo" stash clear

embedded_host="$TMPDIR_TEST/embedded-host"
embedded_repo="$embedded_host/wrapper/deeper/repo"
create_repo "$embedded_host"
create_repo "$embedded_repo"
embedded=()
find_untracked_embedded_repositories "$embedded_host" embedded ||
  fail 'embedded repository scan failed'
[ "${#embedded[@]}" -eq 1 ] || fail 'embedded repository was not detected'
[ "${embedded[0]}" = wrapper/deeper/repo ] ||
  fail 'unexpected embedded repository path'

stash_count_before="$(git -C "$embedded_host" stash list --format='%H' | wc -l)"
if (
  STASHED_SUBMODULES=()
  stash_dirty_submodules "$embedded_host"
) >"$TMPDIR_TEST/embedded.out" 2>"$TMPDIR_TEST/embedded.err"; then
  fail 'auto-stash accepted an embedded repository'
fi
stash_count_after="$(git -C "$embedded_host" stash list --format='%H' | wc -l)"
[ "$stash_count_before" -eq "$stash_count_after" ] ||
  fail 'embedded repository created a stash'

head_commit="$(git -C "$child_repo" rev-parse HEAD)"
head_tree="$(git -C "$child_repo" rev-parse 'HEAD^{tree}')"
empty_tree="$(printf '' | git -C "$child_repo" mktree)"
index_commit="$(
  printf 'synthetic index\n' |
    git -C "$child_repo" \
      -c user.name='Update Test' \
      -c user.email='update-test@example.invalid' \
      commit-tree "$head_tree" -p "$head_commit"
)"
untracked_commit="$(
  printf 'synthetic untracked\n' |
    git -C "$child_repo" \
      -c user.name='Update Test' \
      -c user.email='update-test@example.invalid' \
      commit-tree "$empty_tree"
)"
empty_stash="$(
  printf 'synthetic empty stash\n' |
    git -C "$child_repo" \
      -c user.name='Update Test' \
      -c user.email='update-test@example.invalid' \
      commit-tree "$head_tree" \
      -p "$head_commit" -p "$index_commit" -p "$untracked_commit"
)"

stash_has_expected_graph "$child_repo" "$empty_stash" "$head_commit" ||
  fail 'valid synthetic stash graph was rejected'
payload_status=0
stash_contains_changes "$child_repo" "$empty_stash" || payload_status=$?
[ "$payload_status" -eq 1 ] ||
  fail 'empty stash graph was classified as containing changes'

malformed_stash="$(
  printf 'synthetic malformed stash\n' |
    git -C "$child_repo" \
      -c user.name='Update Test' \
      -c user.email='update-test@example.invalid' \
      commit-tree "$head_tree" -p "$head_commit"
)"
graph_status=0
stash_has_expected_graph \
  "$child_repo" "$malformed_stash" "$head_commit" || graph_status=$?
[ "$graph_status" -eq 1 ] || fail 'malformed stash graph was accepted'

locked_repo="$TMPDIR_TEST/locked"
create_repo "$locked_repo"
printf 'locked\n' >>"$locked_repo/tracked"
locked_git_dir="$(git -C "$locked_repo" rev-parse --absolute-git-dir)"
mkdir -p -- "$locked_git_dir/refs"
: >"$locked_git_dir/refs/stash.lock"
if (
  STASHED_SUBMODULES=()
  stash_dirty_submodules "$locked_repo"
) >"$TMPDIR_TEST/locked.out" 2>"$TMPDIR_TEST/locked.err"; then
  fail 'auto-stash ignored a refs/stash lock failure'
fi
grep -F 'git stash push failed for:' "$TMPDIR_TEST/locked.err" >/dev/null ||
  fail 'stash push failure did not use the updater diagnostic'
[ "$(git -C "$locked_repo" stash list --format='%H' | wc -l)" -eq 0 ] ||
  fail 'failed stash push created a stash'
is_submodule_dirty "$locked_repo" ||
  fail 'failed stash push did not preserve the dirty worktree'

grand_remote="$TMPDIR_TEST/grand-remote"
middle_remote="$TMPDIR_TEST/middle-remote"
outer_repo="$TMPDIR_TEST/outer"
create_repo "$grand_remote"
create_repo "$middle_remote"
git -C "$middle_remote" -c protocol.file.allow=always submodule add -q \
  "$grand_remote" modules/grand
commit_all "$middle_remote" nested-submodule
create_repo "$outer_repo"
git -C "$outer_repo" -c protocol.file.allow=always submodule add -q \
  "$middle_remote" modules/middle
commit_all "$outer_repo" direct-submodule
git -C "$outer_repo" -c protocol.file.allow=always \
  submodule update --init --recursive -q

grand_worktree="$outer_repo/modules/middle/modules/grand"
printf 'grandchild\n' >>"$grand_worktree/tracked"
(
  cd "$outer_repo"
  collect_dirty_submodules
  [ "${#DIRTY_SUBMODULES[@]}" -eq 1 ] ||
    fail 'descendant dirtiness contaminated a parent repository'
  [ "${DIRTY_SUBMODULES[0]}" = modules/middle/modules/grand ] ||
    fail 'dirty grandchild path was not collected'

  STASHED_SUBMODULES=()
  stash_dirty_submodules "${DIRTY_SUBMODULES[@]}" >/dev/null
  [ "${#STASHED_SUBMODULES[@]}" -eq 1 ] ||
    fail 'dirty grandchild was not stashed exactly once'
)
[ "$(git -C "$outer_repo/modules/middle" stash list --format='%H' | wc -l)" -eq 0 ] ||
  fail 'clean parent received a redundant stash'
[ "$(git -C "$grand_worktree" stash list --format='%H' | wc -l)" -eq 1 ] ||
  fail 'dirty grandchild stash was not retained'

if (
  cd "$TMPDIR_TEST"
  collect_dirty_submodules >/dev/null 2>&1
); then
  fail 'submodule enumeration failure was ignored'
fi

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
