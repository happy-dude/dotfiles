#!/usr/bin/env bash
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/report.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/report.sh"
# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/git-worktree.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/git-worktree.sh"
IFS=$'\n\t'

# Matches LOCK_COMMIT_SUBJECT in scripts/update.sh; test_sync_local_branch.sh
# checks that both scripts declare the same subject.
readonly LOCK_COMMIT_SUBJECT="nix: update flake.lock"

usage() {
  printf 'Usage: %s [--validate] <local-branch> <profile> [repository]\n' \
    "${0##*/}" >&2
}

format_worktree() {
  local worktree=$1
  local status

  (
    cd -- "$worktree" || exit 1
    nix fmt .
  ) || return
  status=$(git -C "$worktree" status --porcelain=v1 --untracked-files=all) ||
    return
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

  if ! status=$(git -C "$worktree" status --porcelain=v1 --untracked-files=all); then
    die "unable to read the $label worktree status: $worktree"
    return 1
  fi
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

is_superseded_lock_commit() {
  local repo=$1
  local upstream=$2
  local commit=$3
  local subject
  local paths

  subject=$(git -C "$repo" show -s --format=%s "$commit") || return 1
  [[ $subject == "$LOCK_COMMIT_SUBJECT" ]] || return 1
  paths=$(
    git -C "$repo" diff-tree --no-commit-id --name-only -r "$commit"
  ) || return 1
  [[ $paths == "flake.lock" ]] || return 1
  # Keep any lock commit that adds or removes an input or changes the pinned
  # revision of a branch-local input absent from origin/main's flake.lock.
  python3 - "$repo" "$upstream" "$commit" <<'PY'
import json
import subprocess
import sys

repo, upstream, commit = sys.argv[1:4]


def read_lock(rev: str) -> dict:
    raw = subprocess.check_output(
        ["git", "-C", repo, "show", f"{rev}:flake.lock"],
        text=True,
    )
    data = json.loads(raw)
    if not isinstance(data, dict) or not isinstance(data.get("nodes"), dict):
        raise ValueError("invalid flake.lock")
    return data


try:
    parent = read_lock(f"{commit}^")
    child = read_lock(commit)
    upstream_lock = read_lock(upstream)
except Exception:
    sys.exit(1)

if {k: v for k, v in parent.items() if k != "nodes"} != {
    k: v for k, v in child.items() if k != "nodes"
}:
    sys.exit(1)

p_nodes = parent["nodes"]
c_nodes = child["nodes"]
u_nodes = upstream_lock["nodes"]
if p_nodes.get("root") != c_nodes.get("root"):
    sys.exit(1)

for name in set(p_nodes) | set(c_nodes):
    if p_nodes.get(name) == c_nodes.get(name):
        continue
    if name in u_nodes:
        continue
    if name not in p_nodes or name not in c_nodes:
        sys.exit(1)
    p_locked = p_nodes[name].get("locked")
    c_locked = c_nodes[name].get("locked")
    if not isinstance(p_locked, dict) or not isinstance(c_locked, dict):
        sys.exit(1)
    if (
        p_locked.get("rev") != c_locked.get("rev")
        or p_locked.get("narHash") != c_locked.get("narHash")
    ):
        sys.exit(1)
PY
}

collect_superseded_lock_commits() {
  local repo=$1
  local upstream=$2
  local branch=$3
  local merge_base
  local diff_status
  local cherry_output
  local line
  local marker
  local commit

  merge_base=$(git -C "$repo" merge-base "$upstream" "$branch") || return 1
  # When origin/main has not touched flake.lock since the branch point, local
  # lock updates still apply cleanly and carry the newer lock state.
  if git -C "$repo" diff --quiet "$merge_base" "$upstream" -- flake.lock; then
    return 0
  else
    diff_status=$?
  fi
  ((diff_status == 1)) || return "$diff_status"
  cherry_output=$(git -C "$repo" cherry "$upstream" "$branch") || return 1
  while IFS= read -r line; do
    [[ -n $line ]] || continue
    marker=${line%% *}
    commit=${line#* }
    [[ $marker == + ]] || continue
    if is_superseded_lock_commit "$repo" "$upstream" "$commit"; then
      printf '%s\n' "$commit"
    fi
  done <<<"$cherry_output"
}

report_superseded_lock_commits() {
  local repo=$1
  local commits=$2
  local commit

  [[ -n $commits ]] || return 0
  printf '%s\n' \
    "Local flake.lock updates superseded by origin/main; rebase will omit:" >&2
  while IFS= read -r commit; do
    [[ -n $commit ]] || continue
    git -C "$repo" show -s --format='  %h %s' "$commit" >&2
  done <<<"$commits"
}

rewrite_rebase_todo() {
  local todo_file=$1
  local temp_file="$todo_file.tmp"
  local line
  local sha
  local rest
  local full_sha

  : >"$temp_file" || return 1
  while IFS= read -r line || [[ -n $line ]]; do
    if [[ $line =~ ^(pick|p)[[:space:]]+([0-9a-f]+)(.*)$ ]]; then
      sha=${BASH_REMATCH[2]}
      rest=${BASH_REMATCH[3]}
      full_sha=$(
        git -C "$SYNC_REBASE_WORKTREE" rev-parse --verify "${sha}^{commit}"
      ) || return 1
      if grep -Fqx "$full_sha" <<<"$SYNC_DROP_COMMITS"; then
        printf 'drop %s%s\n' "$sha" "$rest" >>"$temp_file" || return 1
        continue
      fi
    fi
    printf '%s\n' "$line" >>"$temp_file" || return 1
  done <"$todo_file"
  mv -f -- "$temp_file" "$todo_file"
}

rebase_local_branch() {
  local worktree=$1
  local upstream=$2
  local superseded_lock_commits=$3
  local sequence_editor

  # Sync owns only the named branch, not other refs into its history.
  if [[ -z $superseded_lock_commits ]]; then
    git -C "$worktree" -c rebase.updateRefs=false rebase "$upstream"
    return
  fi
  printf -v sequence_editor 'bash %q --rewrite-rebase-todo' "$script_path"
  SYNC_DROP_COMMITS=$superseded_lock_commits \
    SYNC_REBASE_WORKTREE=$worktree \
    GIT_SEQUENCE_EDITOR=$sequence_editor \
    git -C "$worktree" -c rebase.updateRefs=false \
    -c rebase.autoSquash=false -c rebase.abbreviateCommands=false \
    rebase -i --empty=drop --no-autosquash "$upstream"
}

check_flake() {
  local worktree=$1

  (
    cd -- "$worktree" || exit 1
    nix flake check --show-trace --no-update-lock-file
  )
}

build_profile() {
  local worktree=$1
  local profile=$2

  (
    cd -- "$worktree" || exit 1
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
  exit $((128 + $(kill -l "$signal_name")))
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
  local superseded_lock_commits

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

  local rebase_merge rebase_apply
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
    superseded_lock_commits=$(
      collect_superseded_lock_commits \
        "$repo" "$origin_main" "$local_branch_ref"
    )
    report_upstream_equivalent_commits \
      "$repo" "$origin_main" "$local_branch_ref"
    report_superseded_lock_commits "$repo" "$superseded_lock_commits"
    if ! git -C "$main_worktree" merge --ff-only "$origin_main"; then
      printf '%s\n' \
        "Local main could not fast-forward to origin/main." \
        "Inspect with:" >&2
      printf '  git -C %q log --oneline --left-right main...origin/main\n' \
        "$main_worktree" >&2
      printf '%s\n' "Nothing was rebased, pushed, or activated." >&2
      return 1
    fi
    main_head=$(git -C "$main_worktree" rev-parse refs/heads/main)
    if [[ $main_head != "$origin_main" ]]; then
      printf '%s\n' \
        "Local main is ahead of origin/main." \
        "Inspect with:" >&2
      printf '  git -C %q log --oneline --left-right main...origin/main\n' \
        "$main_worktree" >&2
      printf '%s\n' "Nothing was rebased, pushed, or activated." >&2
      return 1
    fi
    if ! rebase_local_branch \
      "$branch_worktree" "$origin_main" "$superseded_lock_commits"; then
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

  git -C "$repo" show-ref --verify --quiet refs/remotes/origin/main ||
    die "origin/main is not fetched; run sync mode first"
  origin_main=$(git -C "$repo" rev-parse refs/remotes/origin/main)
  main_head=$(git -C "$main_worktree" rev-parse refs/heads/main)
  [[ $main_head == "$origin_main" ]] ||
    die "local main does not match origin/main; run sync mode first"
  [[ $(git -C "$branch_worktree" merge-base \
    "$local_branch_ref" "$origin_main") == "$origin_main" ]] ||
    die "$local_branch is not based on origin/main"

  trap 'report_interruption SIGINT' INT
  trap 'report_interruption SIGTERM' TERM
  validation_status=0
  run_phase "Formatting $local_branch" \
    format_worktree "$branch_worktree" &&
    run_phase "Validating flake checks" \
      check_flake "$branch_worktree" &&
    run_phase "Building Home Manager profile $profile" \
      build_profile "$branch_worktree" "$profile" ||
    validation_status=$?
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
  if [[ ${1:-} == --rewrite-rebase-todo ]]; then
    rewrite_rebase_todo "$2"
  else
    main "$@"
  fi
fi
