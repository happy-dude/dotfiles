# shellcheck shell=bash
# Sourced by the portable scripts; it has no shebang of its own.
# Locate the worktree checked out at a branch.
#
# Prints the worktree path and returns 0 when one exists, 1 when the branch
# has no worktree, and 2 when the only registration is prunable. Callers
# distinguish those cases, because a prunable registration needs a different
# remedy than an absent one.

worktree_for_branch() {
  local repo=$1
  local wanted_ref=$2
  local current_worktree=
  local current_branch=
  local current_prunable=false
  local saw_prunable=false
  local field

  while IFS= read -r -d '' field; do
    case $field in
    "worktree "*) current_worktree=${field#worktree } ;;
    "branch "*) current_branch=${field#branch } ;;
    "prunable"*) current_prunable=true ;;
    "")
      if [[ $current_branch == "$wanted_ref" ]]; then
        if [[ $current_prunable == true ]]; then
          saw_prunable=true
        elif git -C "$current_worktree" rev-parse --is-inside-work-tree \
          >/dev/null 2>&1; then
          printf '%s\n' "$current_worktree"
          return 0
        fi
      fi
      current_worktree=
      current_branch=
      current_prunable=false
      ;;
    esac
  done < <(git -C "$repo" worktree list --porcelain -z)

  [[ $saw_prunable == false ]] || return 2
  return 1
}
