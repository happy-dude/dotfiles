#!/usr/bin/env bash
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/git-worktree.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/git-worktree.sh"
IFS=$'\n\t'

usage() {
  printf 'Usage: %s [--validate] <local-branch> <profile> [repository]\n' \
    "${0##*/}" >&2
}

die() {
  printf 'error: %s\n' "$*" >&2
  return 1
}

format_worktree() {
  local worktree=$1
  local status

  (
    cd -- "$worktree"
    nix fmt .
  ) || return
  status=$(git -C "$worktree" status --porcelain=v1 --untracked-files=all)
  if [[ -n $status ]]; then
    printf '%s\n' "$status" >&2
    die "formatter changed the local branch worktree"
    return 1
  fi
}

require_clean_worktree() {
  local worktree=$1
  local label=$2
  local status

  status=$(git -C "$worktree" status --porcelain=v1 --untracked-files=all)
  if [[ -n $status ]]; then
    printf 'Dirty worktree: %s\n' "$worktree" >&2
    printf '%s\n' "$status" >&2
    die "$label worktree is not clean; preflight stopped before fetch or mutation"
    return 1
  fi
}

report_upstream_equivalent_commits() {
  local repo=$1
  local upstream=$2
  local branch=$3
  local line
  local marker
  local commit
  local -a equivalent_commits=()

  while IFS= read -r line; do
    marker=${line%% *}
    commit=${line#* }
    if [[ $marker == - ]]; then
      equivalent_commits+=("$commit")
    fi
  done < <(git -C "$repo" cherry "$upstream" "$branch")

  ((${#equivalent_commits[@]} > 0)) || return 0
  printf '%s\n' \
    "Local commits already represented upstream; rebase is expected to omit:" >&2
  for commit in "${equivalent_commits[@]}"; do
    git -C "$repo" show -s --format='  %h %s' "$commit" >&2
  done
}

check_flake() {
  local worktree=$1

  (
    cd -- "$worktree"
    nix flake check --show-trace --no-update-lock-file
  )
}

build_profile() {
  local worktree=$1
  local profile=$2

  (
    cd -- "$worktree"
    home-manager build --flake ".#$profile" --show-trace \
      --no-out-link --no-update-lock-file
  )
}

run_phase() {
  local label=$1
  shift
  local started=$SECONDS
  local status

  active_phase=$label
  printf '=== %s ===\n' "$label"
  if "$@"; then
    printf '%s\n' \
      "--- $label completed in $((SECONDS - started))s ---"
    return 0
  else
    status=$?
  fi
  printf '%s\n' \
    "--- $label failed after $((SECONDS - started))s ---" >&2
  return "$status"
}

print_validation_command() {
  printf '  %q --validate %q %q %q\n' \
    "$script_path" "$local_branch" "$profile" "$repo" >&2
}

report_interruption() {
  local signal_name=$1

  printf '%s\n' \
    "Interrupted by $signal_name during: $active_phase" \
    "No changes were pushed or activated." >&2
  if [[ $mode == sync ]]; then
    printf '%s\n' \
      "Local main and $local_branch may already contain synchronized commits." \
      "Rerun validation with:" >&2
    print_validation_command
  fi
  exit 130
}

main() {
  mode=sync
  if [[ ${1:-} == --validate ]]; then
    mode=validate
    shift
  fi

  (($# >= 2 && $# <= 3)) || {
    usage
    return 2
  }
  local_branch=$1
  profile=$2
  repo=${3:-"$HOME/dotfiles"}
  script_path=$(readlink -f "$0")
  active_phase=preflight
  local local_branch_ref="refs/heads/$local_branch"
  local branch_worktree
  local main_worktree
  local lookup_status
  local upstream
  local origin_main
  local main_head

  [[ $local_branch != main ]] || die "local branch must not be main"
  git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1 ||
    die "not a Git worktree: $repo"
  repo=$(git -C "$repo" rev-parse --path-format=absolute --show-toplevel)

  if branch_worktree=$(worktree_for_branch "$repo" "$local_branch_ref"); then
    :
  else
    lookup_status=$?
    if ((lookup_status == 2)); then
      die "worktree registration is prunable for branch: $local_branch"
    else
      die "worktree not found for branch: $local_branch"
    fi
    return 1
  fi
  if main_worktree=$(worktree_for_branch "$repo" refs/heads/main); then
    :
  else
    lookup_status=$?
    if ((lookup_status == 2)); then
      die "main worktree registration is prunable"
    else
      die "main worktree not found"
    fi
    return 1
  fi

  upstream=$(
    git -C "$repo" for-each-ref --format='%(upstream)' "$local_branch_ref"
  )
  [[ -z $upstream ]] ||
    die "$local_branch must remain local-only; found upstream: $upstream"

  rebase_merge=$(
    git -C "$branch_worktree" rev-parse --path-format=absolute \
      --git-path rebase-merge
  )
  rebase_apply=$(
    git -C "$branch_worktree" rev-parse --path-format=absolute \
      --git-path rebase-apply
  )
  [[ ! -d $rebase_merge && ! -d $rebase_apply ]] ||
    die "finish or abort the active rebase before running this script"
  require_clean_worktree "$branch_worktree" "$local_branch"
  require_clean_worktree "$main_worktree" main

  if [[ $mode == sync ]]; then
    git -C "$repo" fetch origin \
      refs/heads/main:refs/remotes/origin/main
    origin_main=$(git -C "$repo" rev-parse refs/remotes/origin/main)
    report_upstream_equivalent_commits \
      "$repo" "$origin_main" "$local_branch_ref"
    if ! git -C "$main_worktree" merge --ff-only "$origin_main"; then
      printf '%s\n' \
        "Local main could not fast-forward to origin/main." \
        "Inspect with:" >&2
      printf '  git -C %q log --oneline --left-right main...origin/main\n' \
        "$main_worktree" >&2
      printf '%s\n' "Nothing was rebased, pushed, or activated." >&2
      return 1
    fi
    if ! git -C "$branch_worktree" rebase "$origin_main"; then
      printf '%s\n' \
        "Rebase stopped before completion. Nothing was pushed or activated." \
        "Local main was fast-forwarded to $origin_main." \
        "Resolve conflicts in: $branch_worktree" \
        "Then continue with:" >&2
      printf '  git -C %q rebase --continue\n' "$branch_worktree" >&2
      printf '%s\n' "After completion, validate with:" >&2
      print_validation_command
      printf '%s\n' "To restore the prior local branch, run:" >&2
      printf '  git -C %q rebase --abort\n' "$branch_worktree" >&2
      return 1
    fi
  fi

  origin_main=$(git -C "$repo" rev-parse refs/remotes/origin/main)
  main_head=$(git -C "$main_worktree" rev-parse refs/heads/main)
  [[ $main_head == "$origin_main" ]] ||
    die "local main does not match origin/main; run sync mode first"
  [[ $(git -C "$branch_worktree" merge-base \
    "$local_branch_ref" "$origin_main") == "$origin_main" ]] ||
    die "$local_branch is not based on origin/main"

  trap 'report_interruption SIGINT' INT
  trap 'report_interruption SIGTERM' TERM
  if run_phase "Formatting $local_branch" \
    format_worktree "$branch_worktree"; then
    if run_phase "Validating flake checks" \
      check_flake "$branch_worktree"; then
      if run_phase "Building Home Manager profile $profile" \
        build_profile "$branch_worktree" "$profile"; then
        validation_status=0
      else
        validation_status=$?
      fi
    else
      validation_status=$?
    fi
  else
    validation_status=$?
  fi
  trap - INT TERM

  if ((validation_status != 0)); then
    printf '%s\n' \
      "Validation failed during: $active_phase" \
      "No changes were pushed or activated." >&2
    if [[ $mode == sync ]]; then
      printf '%s\n' \
        "Local main and $local_branch were synchronized before validation failed." >&2
    else
      printf '%s\n' \
        "Validation mode performed no fetch or rebase." >&2
    fi
    printf '%s\n' "Rerun validation with:" >&2
    print_validation_command
    return "$validation_status"
  fi

  if [[ $mode == sync ]]; then
    printf '%s\n' \
      "Local main and $local_branch now follow origin/main." \
      "Nothing was pushed. The named branch remains local."
  else
    printf '%s\n' \
      "Validated local main and $local_branch against origin/main." \
      "Validation mode performed no fetch, rebase, push, or activation."
  fi
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
  main "$@"
fi
