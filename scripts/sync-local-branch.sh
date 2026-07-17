#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

mode=sync
if [[ ${1:-} == --validate ]]; then
  mode=validate
  shift
fi

usage() {
  printf 'Usage: %s [--validate] <local-branch> <profile> [repository]\n' \
    "${0##*/}" >&2
}

die() {
  printf 'error: %s\n' "$*" >&2
  return 1
}

worktree_for_branch() {
  local repo=$1
  local wanted_ref=$2
  local current_worktree=
  local current_branch=

  while IFS= read -r line; do
    case $line in
    "worktree "*) current_worktree=${line#worktree } ;;
    "branch "*) current_branch=${line#branch } ;;
    "")
      if [[ $current_branch == "$wanted_ref" ]]; then
        printf '%s\n' "$current_worktree"
        return 0
      fi
      current_worktree=
      current_branch=
      ;;
    esac
  done < <(
    git -C "$repo" worktree list --porcelain
    printf '\n'
  )
  return 1
}

(($# >= 2 && $# <= 3)) || {
  usage
  exit 2
}
local_branch=$1
profile=$2
repo=${3:-"$HOME/dotfiles"}
[[ $local_branch != main ]] || die "local branch must not be main"

git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1 ||
  die "not a Git worktree: $repo"
branch_worktree=$(worktree_for_branch "$repo" "refs/heads/$local_branch") ||
  die "worktree not found for branch: $local_branch"
main_worktree=$(worktree_for_branch "$repo" refs/heads/main) ||
  die "main worktree not found"
rebase_merge=$(git -C "$branch_worktree" rev-parse --git-path rebase-merge)
rebase_apply=$(git -C "$branch_worktree" rev-parse --git-path rebase-apply)
[[ ! -d $rebase_merge && ! -d $rebase_apply ]] ||
  die "finish or abort the active rebase before running this script"

[[ -z $(git -C "$branch_worktree" status --porcelain=v1 --untracked-files=all) ]] ||
  die "$local_branch worktree is not clean"
[[ -z $(git -C "$main_worktree" status --porcelain=v1 --untracked-files=all) ]] ||
  die "main worktree is not clean"

if [[ $mode == sync ]]; then
  git -C "$repo" fetch origin main
  git -C "$main_worktree" merge --ff-only origin/main
  if ! git -C "$branch_worktree" rebase origin/main; then
    printf '%s\n' \
      "Rebase stopped with conflicts. Nothing was pushed or activated." \
      "Resolve each conflict, stage the corrected files, and run" \
      "git rebase --continue until complete. Then validate with:" \
      "$0 --validate $local_branch $profile $repo" \
      "To restore the prior local branch, run: git rebase --abort" >&2
    exit 1
  fi
fi

[[ $(git -C "$branch_worktree" merge-base "$local_branch" origin/main) == $(git -C "$branch_worktree" rev-parse origin/main) ]] ||
  die "$local_branch is not based on origin/main"

(
  cd -- "$branch_worktree"
  nix fmt .
  git diff --exit-code
  git diff --cached --exit-code
  nix flake check --show-trace --no-update-lock-file
  home-manager build --flake ".#$profile" --show-trace \
    --no-out-link --no-update-lock-file
)

printf '%s\n' \
  "Local main and $local_branch now follow origin/main." \
  "Nothing was pushed. The named branch remains local."
