#!/usr/bin/env bash

###################################################################################################
###################################################################################################
### Update script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: Fri Mar 07 2026
###
### Modes:
###     check   Validate the flake and build the selected Home Manager configuration without applying it
###     apply   Validate, build, and activate the existing lock file
###     update  Run the update workflow below, validate it, and optionally activate it (default)
###
### Update workflow:
###     1. Update the main dotfiles repository
###     2. Sync / init / update configured git submodules, if any
###     3. Run `nix fmt .`
###     4. Run `nix flake update`
###     5. Validate the flake and build the Home Manager configuration
###     6. Optionally run `home-manager switch`
###     7. Report the generation closure changes and repository shortlog
###
### Usage:
###     ./scripts/update.sh [options] [directory]
###
### Notes:
###     - Dirty top-level repo pull is allowed via:
###           git pull --rebase --autostash
###
###     - Configured dirty submodules are NOT updated unless either:
###           1. you clean them up yourself, or
###           2. you pass --autostash-submodules
###
###     - When .gitmodules exists, set tracked branches with:
###           git submodule set-branch --branch <branch> <path>
###
###################################################################################################
###################################################################################################

set -euo pipefail

#--------------------------------------------------------------------------------------------------
# Defaults / globals
#--------------------------------------------------------------------------------------------------

AUTO_STASH_SUBMODULES=0
MODE=update
MODE_SET=0
SKIP_PULL=0
SKIP_SUBMODULES=0
SKIP_STATUS=0
SKIP_NIX_FMT=0
SKIP_NIX_FLAKE=0
SKIP_HOME_MANAGER=0

SHOW_CHANGES=0
case "${VERBOSE:-0}" in
1 | true | yes) VERBOSE=1 ;;
*) VERBOSE=0 ;;
esac

REPO_DIR="."
REPO_DIR_SET=0
DIRTY_SUBMODULES=()
STASHED_SUBMODULES=()

SECTION_NAME=""
SECTION_START=0
SCRIPT_START_SECS=$SECONDS
SCRIPT_START_TS="$(date '+%Y-%m-%d %I:%M:%S %p %Z')"

SECTION_LABELS=()
SECTION_RESULTS=()
SECTION_TIMES=()

UPDATE_START_GIT_HEAD=""

# Default to the flake output matching the current user (schan, stachan, ...),
# so each machine switches its own config. Override with HOME_MANAGER_FLAKE.
HOME_MANAGER_FLAKE="${HOME_MANAGER_FLAKE:-.#$(whoami)}"

#--------------------------------------------------------------------------------------------------
# Usage
#--------------------------------------------------------------------------------------------------

usage() {
  cat <<'EOF'
Usage:
  ./scripts/update.sh [update|check|apply] [options] [directory]

Modes:
  update
      Run the full update workflow, validate it, then activate it unless
      --skip-home-manager is set. This is the default mode.

  check
      Run non-mutating flake validation and build the selected Home Manager
      configuration without changing the active profile or lock file.

  apply
      Run the same validation/build as check, then activate the selected
      Home Manager configuration without updating the lock file.

Options:

  General:
    --verbose
        Enable verbose script output, including --show-changes.

    --quiet
        Reduce script output (default).

    --show-changes
        After activation, show the committed Git diff when HEAD advanced.
        If HEAD is unchanged, show the staged diff when one exists.

    -h, --help
        Show this help.

  Git / dotfiles:
    --skip-pull
        Skip `git pull --rebase --autostash`.

    --skip-submodules
        Skip submodule sync/init/check/update steps. These steps are already
        skipped when the repository has no configured submodules.

    --skip-status
        Skip `git submodule status --recursive` when submodules are configured.

    --autostash-submodules
        Automatically stash dirty submodules before updating them.
        Retain the stashes for explicit review instead of applying them.

  Nix / Home Manager:
    --skip-nix-fmt
        Skip `nix fmt .`.

    --skip-nix-flake
        Skip `nix flake update`.

    --skip-home-manager
        Skip Home Manager activation after a successful validation/build.

Environment:
  VERBOSE=1
      Enable verbose output, including --show-changes.

  VERBOSE=0
      Quiet output (default).

  HOME_MANAGER_FLAKE=...
      Flake target for home-manager. Default: .#<current user> (e.g. .#schan, .#stachan)
EOF
}

#--------------------------------------------------------------------------------------------------
# Small helpers
#--------------------------------------------------------------------------------------------------

have() {
  command -v "$1" >/dev/null 2>&1
}

msg() {
  printf '%s\n' "$*"
}

vmsg() {
  if [ "${VERBOSE:-0}" -eq 1 ]; then
    printf '%s\n' "$*"
  fi
}

resolve_output_options() {
  if [ "$VERBOSE" -eq 1 ]; then
    SHOW_CHANGES=1
  fi
}

warn() {
  printf 'Warning: %s\n' "$*" >&2
}

# Callers here do not test a return value, so a fatal condition has to end
# the run rather than let a later step proceed on bad state.
die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

section_start() {
  SECTION_NAME="$1"
  SECTION_START=$SECONDS
  printf -- '\n=== %s ===\n' "$SECTION_NAME"
}

section_end() {
  local result="${1:-done}"
  local elapsed
  elapsed=$((SECONDS - SECTION_START))

  SECTION_LABELS+=("$SECTION_NAME")
  SECTION_RESULTS+=("$result")
  SECTION_TIMES+=("$elapsed")

  printf -- '--- %s completed in %ss ---\n' "$SECTION_NAME" "$elapsed"
}

print_timing_summary() {
  local total
  local i

  total=$((SECONDS - SCRIPT_START_SECS))

  msg
  msg "Timing summary:"
  printf '  %-56s %-8s %7s\n' "Section" "Result" "Time"
  printf '  %-56s %-8s %7s\n' "--------------------------------------------------------" "--------" "-------"

  for i in "${!SECTION_LABELS[@]}"; do
    printf '  %-56s %-8s %6ss\n' \
      "${SECTION_LABELS[$i]}" \
      "${SECTION_RESULTS[$i]}" \
      "${SECTION_TIMES[$i]}"
  done

  printf '  %-56s %-8s %6ss\n' "Total" "-" "$total"
}

print_final_summary() {
  local end_ts="$1"
  local total_elapsed="$2"
  local done_count=0
  local skipped_count=0
  local failed_count=0
  local i

  for i in "${!SECTION_RESULTS[@]}"; do
    case "${SECTION_RESULTS[$i]}" in
    done)
      done_count=$((done_count + 1))
      ;;
    skipped)
      skipped_count=$((skipped_count + 1))
      ;;
    failed)
      failed_count=$((failed_count + 1))
      ;;
    esac
  done

  msg
  msg "Summary:"
  printf '  %-14s %s\n' "Started at:" "$SCRIPT_START_TS"
  printf '  %-14s %s\n' "Ended at:" "$end_ts"
  printf '  %-14s %ss\n' "Elapsed:" "$total_elapsed"
  printf '  %-14s %s\n' "Repository:" "$(pwd)"
  printf '  %-14s %s\n' "Sections:" "done=$done_count skipped=$skipped_count failed=$failed_count"
  printf '  %-14s %s\n' "Review:" "git status --short; git diff"

  if [ "${#STASHED_SUBMODULES[@]}" -gt 0 ]; then
    printf '  %-14s %s\n' "Auto-stashed:" "${#STASHED_SUBMODULES[@]} submodule(s)"
    for i in "${!STASHED_SUBMODULES[@]}"; do
      printf '  %-14s %s\n' "" "${STASHED_SUBMODULES[$i]}"
    done
    printf '  %-14s %s\n' "Stash review:" "git -C <submodule-path> stash list"
  fi
}

finish_successfully() {
  local end_ts
  local total_elapsed

  end_ts="$(date '+%Y-%m-%d %I:%M:%S %p %Z')"
  total_elapsed=$((SECONDS - SCRIPT_START_SECS))

  msg
  msg "=== Done! ==="

  print_final_summary "$end_ts" "$total_elapsed"
  print_timing_summary
}

abort_failed_section() {
  local result="$1"

  if [ "$result" = "failed" ]; then
    warn "Aborting before validation or activation because a required update step failed"
    exit 1
  fi
}

run_flake_validation() {
  local status=0

  section_start "Validating flake checks"

  if ! have nix; then
    warn "Cannot validate the flake (nix not found)"
    section_end "failed"
    return 127
  fi

  nix flake check --show-trace --no-update-lock-file || status=$?

  if [ "$status" -ne 0 ]; then
    warn "nix flake check failed (exit code: $status)"
    section_end "failed"
    return "$status"
  fi

  section_end "done"
}

run_home_manager_build() {
  local status=0

  section_start "Building Home Manager configuration without activation"

  if ! have home-manager; then
    warn "Cannot build the Home Manager configuration (home-manager not found)"
    section_end "failed"
    return 127
  fi

  home-manager build \
    --flake "$HOME_MANAGER_FLAKE" \
    --show-trace \
    --no-out-link \
    --no-update-lock-file || status=$?

  if [ "$status" -ne 0 ]; then
    warn "home-manager build failed (exit code: $status)"
    section_end "failed"
    return "$status"
  fi

  section_end "done"
}

run_validation() {
  run_flake_validation || return
  run_home_manager_build || return
}

# After a validated update, the refreshed lock is the update's deliverable;
# leaving it uncommitted strands it in the working tree. Bookkeeping failure
# must not undo a validated run, so this only warns. Commits flake.lock alone,
# even when the tree carries unrelated changes.
commit_flake_lock() {
  local target="$1"

  have git || {
    warn "Cannot commit flake.lock (git not found)"
    return 0
  }

  if [ -z "$(git -C "$target" status --porcelain -- flake.lock)" ]; then
    return 0
  fi

  if git -C "$target" commit -q -m "nix: update flake.lock" -- flake.lock; then
    printf 'Committed the refreshed flake.lock as "nix: update flake.lock".\n'
  else
    warn "flake.lock is updated but could not be committed; commit it manually"
  fi
}

current_home_manager_generation() {
  local profile

  profile="${XDG_STATE_HOME:-$HOME/.local/state}/nix/profiles/home-manager"
  [ -L "$profile" ] || return 1
  readlink -f -- "$profile"
}

print_git_changes() {
  local git_before="$1"
  local git_after="$2"
  local show_changes="$3"

  [ "$show_changes" -eq 1 ] || return 1

  if [ -n "$git_before" ] &&
    [ -n "$git_after" ] &&
    [ "$git_before" != "$git_after" ] &&
    git cat-file -e "${git_before}^{commit}" 2>/dev/null &&
    git cat-file -e "${git_after}^{commit}" 2>/dev/null; then
    msg
    msg "Committed Git changes:"
    git --no-pager diff --no-ext-diff "${git_before}..${git_after}" --
    return 0
  fi

  local staged_status=0
  git diff --cached --quiet -- || staged_status=$?
  if [ "$staged_status" -gt 1 ]; then
    warn "could not inspect staged changes"
  elif [ "$staged_status" -eq 1 ]; then
    msg
    msg "Staged Git changes:"
    git --no-pager diff --cached --no-ext-diff --
    return 0
  fi

  msg
  msg "No committed or staged Git changes to show."
  return 1
}

print_generation_changelog() {
  local generation_before="$1"
  local generation_after="$2"
  local git_before="$3"
  local git_after="$4"
  local show_changes="$5"
  local shortlog=""
  local reported=0

  section_start "Home Manager generation changelog"

  if [ -n "$generation_before" ] &&
    [ -n "$generation_after" ] &&
    [ "$generation_before" != "$generation_after" ]; then
    printf 'Previous: %s\n' "$generation_before"
    printf 'Current:  %s\n' "$generation_after"
    msg
    msg "Nix closure changes:"
    if have nix; then
      if ! nix store diff-closures "$generation_before" "$generation_after"; then
        warn "Could not compare the Home Manager generation closures"
      fi
    else
      warn "Cannot compare Home Manager generations (nix not found)"
    fi
    reported=1
  elif [ -n "$generation_before" ] &&
    [ "$generation_before" = "$generation_after" ]; then
    msg "Home Manager generation is unchanged: $generation_after"
  else
    warn "Could not resolve both Home Manager generation paths"
  fi

  if [ -n "$git_before" ] &&
    [ -n "$git_after" ] &&
    [ "$git_before" != "$git_after" ] &&
    git cat-file -e "${git_before}^{commit}" 2>/dev/null &&
    git cat-file -e "${git_after}^{commit}" 2>/dev/null; then
    if shortlog="$(
      git shortlog --format='%h %s' --no-merges \
        "${git_before}..${git_after}"
    )" && [ -n "$shortlog" ]; then
      msg
      msg "Repository shortlog:"
      printf '%s\n' "$shortlog"
      reported=1
    fi
  fi

  if print_git_changes "$git_before" "$git_after" "$show_changes"; then
    reported=1
  fi

  if [ "$reported" -eq 1 ]; then
    section_end "done"
  else
    section_end "skipped"
  fi
}

run_home_manager_switch() {
  local status=0
  local generation_before
  local generation_after
  local git_after

  generation_before="$(current_home_manager_generation || true)"

  section_start "Running home-manager switch"

  if ! have home-manager; then
    warn "Cannot activate the Home Manager configuration (home-manager not found)"
    section_end "failed"
    return 127
  fi

  home-manager switch \
    --flake "$HOME_MANAGER_FLAKE" \
    --show-trace \
    --no-update-lock-file || status=$?

  if [ "$status" -ne 0 ]; then
    warn "home-manager switch failed (exit code: $status)"
    section_end "failed"
    return "$status"
  fi

  section_end "done"

  generation_after="$(current_home_manager_generation || true)"
  git_after="$(git rev-parse --verify HEAD 2>/dev/null || true)"
  print_generation_changelog \
    "$generation_before" \
    "$generation_after" \
    "$UPDATE_START_GIT_HEAD" \
    "$git_after" \
    "$SHOW_CHANGES"
}

#--------------------------------------------------------------------------------------------------
# Git / submodule helpers
#--------------------------------------------------------------------------------------------------

has_configured_submodules() {
  [ -f .gitmodules ] &&
    git config -f .gitmodules --get-regexp '^submodule\..*\.path$' >/dev/null 2>&1
}

# `git diff --quiet` exits 0 when there is no difference, 1 when there is one,
# and 2 or more when the comparison itself failed. The functions below keep
# those apart deliberately: collapsing them into `if ! git diff --quiet` would
# report a failed comparison as a dirty tree, or a broken repository as a clean
# one, and both mislead the stash guard that decides whether to move a user's
# uncommitted work.
is_submodule_dirty() {
  local path="$1"
  local status
  local untracked

  if git -C "$path" diff --quiet --ignore-submodules=all --; then
    :
  else
    status=$?
    [ "$status" -eq 1 ] && return 0
    return "$status"
  fi

  if git -C "$path" diff --cached --quiet --; then
    :
  else
    status=$?
    [ "$status" -eq 1 ] && return 0
    return "$status"
  fi

  if ! untracked="$(git -C "$path" ls-files --others --exclude-standard)"; then
    return 2
  fi

  [ -n "$untracked" ]
}

collect_dirty_submodules() {
  local path
  local paths
  local status

  DIRTY_SUBMODULES=()

  # shellcheck disable=SC2016 # Expanded by git submodule foreach's shell.
  if ! paths="$(git submodule foreach --quiet --recursive \
    'printf "%s\n" "$displaypath"')"; then
    warn "failed to enumerate initialized submodules"
    return 2
  fi

  while IFS= read -r path; do
    [ -n "$path" ] || continue
    if is_submodule_dirty "$path"; then
      DIRTY_SUBMODULES+=("$path")
    else
      status=$?
      if [ "$status" -ne 1 ]; then
        warn "failed to inspect submodule state: $path"
        return "$status"
      fi
    fi
  done <<<"$paths"
}

update_submodule_graph() {
  # Advance only submodules recorded by the root repository. Applying
  # --remote recursively would also move descendants away from the gitlinks
  # recorded by their direct parents.
  git submodule update --remote

  # A direct update may replace a parent's .gitmodules. Refresh nested URLs
  # before initializing and aligning the resulting descendant graph.
  git submodule sync --recursive

  # Intentionally top-level-only foreach. The inner update aligns every
  # descendant to its direct parent's gitlink and honors update=none. Do not
  # add --remote or --checkout here.
  git submodule foreach --quiet '
    git submodule update --init --recursive
  '
}

find_untracked_embedded_repositories() {
  local path="$1"
  local result_name="$2"
  local repo_root
  local record
  local candidate
  local candidate_root
  local status_file
  local -n results="$result_name"

  results=()

  if ! repo_root="$(git -C "$path" rev-parse --show-toplevel)"; then
    return 2
  fi

  if ! status_file="$(mktemp "${TMPDIR:-/tmp}/update-embedded.XXXXXX")"; then
    return 2
  fi

  if ! git -C "$path" status \
    --porcelain=v1 -z \
    --untracked-files=all \
    --ignore-submodules=all >"$status_file"; then
    rm -f -- "$status_file"
    return 2
  fi

  while IFS= read -r -d '' record; do
    [[ $record == "?? "* ]] || continue
    candidate=${record:3}
    candidate=${candidate%/}
    [ -d "$path/$candidate" ] || continue

    candidate_root="$(
      git -C "$path/$candidate" rev-parse --show-toplevel 2>/dev/null || true
    )"
    if [ -n "$candidate_root" ] && [ "$candidate_root" != "$repo_root" ]; then
      results+=("$candidate")
    fi
  done <"$status_file"

  rm -f -- "$status_file"
}

stash_has_expected_graph() {
  local path="$1"
  local stash="$2"
  local expected_head="$3"
  local parents
  local head_parent
  local index_parent
  local untracked_parent
  local extra_parent
  local index_parents
  local untracked_parents

  if ! parents="$(git -C "$path" show -s --format=%P "$stash")"; then
    return 2
  fi

  IFS=' ' read -r \
    head_parent index_parent untracked_parent extra_parent <<<"$parents"

  if [ -z "$head_parent" ] ||
    [ -z "$index_parent" ] ||
    [ -z "$untracked_parent" ] ||
    [ -n "$extra_parent" ] ||
    [ "$head_parent" != "$expected_head" ]; then
    return 1
  fi

  if ! index_parents="$(
    git -C "$path" show -s --format=%P "$index_parent"
  )"; then
    return 2
  fi
  [ "$index_parents" = "$expected_head" ] || return 1

  if ! untracked_parents="$(
    git -C "$path" show -s --format=%P "$untracked_parent"
  )"; then
    return 2
  fi
  [ -z "$untracked_parents" ] || return 1
}

# Exit codes here mean the same as in is_submodule_dirty above.
stash_contains_changes() {
  local path="$1"
  local stash="$2"
  local status
  local untracked

  if git -C "$path" diff --quiet "${stash}^1" "${stash}^2" --; then
    :
  else
    status=$?
    [ "$status" -eq 1 ] && return 0
    return "$status"
  fi

  if git -C "$path" diff --quiet "${stash}^2" "$stash" --; then
    :
  else
    status=$?
    [ "$status" -eq 1 ] && return 0
    return "$status"
  fi

  if ! untracked="$(
    git -C "$path" ls-tree -r --name-only "${stash}^3"
  )"; then
    return 2
  fi
  [ -n "$untracked" ] && return 0

  return 1
}

stash_dirty_submodules() {
  local path
  local expected_head
  local stash_before
  local stash_after
  local stash_status
  local status
  local candidate
  local -a embedded_repositories

  for path in "$@"; do
    [ -n "$path" ] || continue

    if is_submodule_dirty "$path"; then
      :
    else
      status=$?
      [ "$status" -eq 1 ] && continue
      die "failed to inspect submodule before auto-stashing: $path"
    fi

    if ! find_untracked_embedded_repositories \
      "$path" embedded_repositories; then
      die "failed to inspect embedded repositories in: $path"
    fi

    if [ "${#embedded_repositories[@]}" -gt 0 ]; then
      warn "cannot auto-stash untracked embedded Git repositories in: $path"
      for candidate in "${embedded_repositories[@]}"; do
        printf '  - %s/%s\n' "$path" "$candidate" >&2
      done
      die "move, remove, ignore, or register the embedded repositories before updating"
    fi

    if ! expected_head="$(git -C "$path" rev-parse --verify HEAD)"; then
      die "failed to resolve submodule HEAD before auto-stashing: $path"
    fi
    stash_before="$(git -C "$path" rev-parse --verify -q refs/stash || true)"

    msg "Stashing dirty submodule: $path"
    if git -C "$path" stash push -u -m "update.sh auto-stash"; then
      stash_status=0
    else
      stash_status=$?
    fi
    stash_after="$(git -C "$path" rev-parse --verify -q refs/stash || true)"

    if [ "$stash_status" -ne 0 ]; then
      if [ -n "$stash_after" ] && [ "$stash_after" != "$stash_before" ]; then
        warn "retained stash created before failure: $stash_after ($path)"
      fi
      die "git stash push failed for: $path (exit code: $stash_status)"
    fi

    if [ -z "$stash_after" ] || [ "$stash_after" = "$stash_before" ]; then
      die "Git reported success but did not create a stash for: $path"
    fi

    if stash_has_expected_graph "$path" "$stash_after" "$expected_head"; then
      :
    else
      status=$?
      warn "retained auto-stash for review: $stash_after ($path)"
      if [ "$status" -eq 1 ]; then
        die "new auto-stash has an unexpected graph: $path"
      fi
      die "could not inspect new auto-stash graph: $path"
    fi

    if stash_contains_changes "$path" "$stash_after"; then
      :
    else
      status=$?
      warn "retained auto-stash for review: $stash_after ($path)"
      if [ "$status" -eq 1 ]; then
        die "new auto-stash has no verified payload: $path"
      fi
      die "could not inspect new auto-stash payload: $path"
    fi

    if is_submodule_dirty "$path"; then
      warn "auto-stash did not leave the submodule clean: $path"
      git -C "$path" --no-pager status --short --untracked-files=all >&2
      die "resolve the remaining submodule state before updating"
    else
      status=$?
      if [ "$status" -ne 1 ]; then
        die "failed to verify submodule state after auto-stashing: $path"
      fi
    fi

    STASHED_SUBMODULES+=("$path")
  done
}

# Everything below runs only when this file is executed. The test suites
# source it to exercise the functions above without performing an update.
main() {
  local section_result status

  # Argument parsing
  #--------------------------------------------------------------------------------------------------

  while [ $# -gt 0 ]; do
    case "$1" in
    check | apply | update)
      if [ "$MODE_SET" -eq 1 ]; then
        die "multiple modes specified: $MODE and $1"
      fi
      MODE="$1"
      MODE_SET=1
      ;;
    --verbose)
      VERBOSE=1
      ;;
    --quiet)
      VERBOSE=0
      ;;
    --show-changes)
      SHOW_CHANGES=1
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    --skip-pull)
      SKIP_PULL=1
      ;;
    --skip-submodules)
      SKIP_SUBMODULES=1
      ;;
    --skip-status)
      SKIP_STATUS=1
      ;;
    --autostash-submodules)
      AUTO_STASH_SUBMODULES=1
      ;;
    --skip-nix-fmt)
      SKIP_NIX_FMT=1
      ;;
    --skip-nix-flake)
      SKIP_NIX_FLAKE=1
      ;;
    --skip-home-manager)
      SKIP_HOME_MANAGER=1
      ;;
    -*)
      die "unknown option: $1"
      ;;
    *)
      if [ "$REPO_DIR_SET" -eq 1 ]; then
        die "multiple repository directories specified: $REPO_DIR and $1"
      fi
      REPO_DIR="$1"
      REPO_DIR_SET=1
      ;;
    esac
    shift
  done

  resolve_output_options

  if [ "$MODE" = "apply" ] && [ "$SKIP_HOME_MANAGER" -eq 1 ]; then
    die "apply mode cannot be combined with --skip-home-manager; use check mode instead"
  fi

  # Basic checks
  #--------------------------------------------------------------------------------------------------

  cd "$REPO_DIR"

  if ! have git; then
    die "git not found in PATH"
  fi

  if ! git rev-parse --git-dir >/dev/null 2>&1; then
    die "not a git repository"
  fi

  UPDATE_START_GIT_HEAD="$(git rev-parse --verify HEAD)"

  msg "=== Dotfiles $MODE ==="
  msg "Started at: $SCRIPT_START_TS"

  case "$MODE" in
  check)
    run_validation || exit $?
    finish_successfully
    exit 0
    ;;
  apply)
    run_validation || exit $?
    run_home_manager_switch || exit $?
    commit_flake_lock "$PWD"
    finish_successfully
    exit 0
    ;;
  update)
    ;;
  *)
    die "internal error: unsupported mode $MODE"
    ;;
  esac

  #--------------------------------------------------------------------------------------------------
  # 1. Updating the main dotfiles repository
  #--------------------------------------------------------------------------------------------------

  if [ "$SKIP_PULL" -eq 0 ]; then
    section_start "Pulling latest changes for main repo"
    if git rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; then
      vmsg "Dirty top-level worktree is allowed here via --autostash."
      git pull --rebase --autostash
      section_end "done"
    else
      warn "No upstream tracking branch configured; skipping pull (use --skip-pull to suppress this warning)"
      section_end "skipped"
    fi
  else
    section_start "Skipping main repo pull (--skip-pull)"
    section_end "skipped"
  fi

  #--------------------------------------------------------------------------------------------------
  # 2. Sync / init / update git submodules
  #--------------------------------------------------------------------------------------------------

  if ! has_configured_submodules; then
    section_start "Skipping submodule operations (none configured)"
    section_end "skipped"
  elif [ "$SKIP_SUBMODULES" -eq 0 ]; then
    section_start "Syncing submodule URLs"
    git submodule sync --recursive
    section_end "done"

    section_start "Initializing submodules"
    git submodule update --init --recursive
    section_end "done"

    section_start "Checking submodules for local changes"

    if ! collect_dirty_submodules; then
      die "failed to collect dirty submodules"
    fi

    if [ "${#DIRTY_SUBMODULES[@]}" -gt 0 ]; then
      if [ "$AUTO_STASH_SUBMODULES" -eq 1 ]; then
        msg "Dirty submodules detected; auto-stashing them now..."
        stash_dirty_submodules "${DIRTY_SUBMODULES[@]}"
      else
        warn "dirty submodules detected:"
        for path in "${DIRTY_SUBMODULES[@]}"; do
          printf '  - %s\n' "$path" >&2
        done
        printf '\n' >&2
        warn "top-level repo pull is allowed when dirty"
        warn "but dirty submodules must be committed, stashed, or reset first"
        warn "rerun with: --autostash-submodules"
        exit 1
      fi
    else
      vmsg "No dirty submodules found."
    fi

    section_end "done"

    section_start "Updating direct submodules and pinning descendants"
    update_submodule_graph
    section_end "done"
  else
    section_start "Skipping submodule operations (--skip-submodules)"
    section_end "skipped"
  fi

  if has_configured_submodules; then
    if [ "$SKIP_STATUS" -eq 1 ]; then
      section_start "Skipping submodule status (--skip-status)"
      section_end "skipped"
    else
      section_start "Submodule status"
      git submodule status --recursive
      section_end "done"
    fi
  fi

  #--------------------------------------------------------------------------------------------------
  # 3. Run `nix fmt .`
  #--------------------------------------------------------------------------------------------------

  if [ "$SKIP_NIX_FMT" -eq 1 ]; then
    section_start "Skipping nix fmt (--skip-nix-fmt)"
    section_end "skipped"
  else
    section_start "Formatting flake with nix fmt"

    section_result="done"
    if have nix; then
      status=0
      nix fmt . || status=$?

      if [ "$status" -ne 0 ]; then
        warn "nix fmt failed (exit code: $status)"
        section_result="failed"
      fi
    else
      warn "Cannot format the flake (nix not found)"
      section_result="failed"
    fi

    section_end "$section_result"
    abort_failed_section "$section_result"
  fi

  #--------------------------------------------------------------------------------------------------
  # 4. Run `nix flake update`
  #--------------------------------------------------------------------------------------------------

  if [ "$SKIP_NIX_FLAKE" -eq 1 ]; then
    section_start "Skipping nix flake update (--skip-nix-flake)"
    section_end "skipped"
  else
    section_start "Updating flake inputs"

    section_result="done"
    if have nix; then
      status=0
      nix flake update || status=$?

      if [ "$status" -ne 0 ]; then
        warn "nix flake update failed (exit code: $status)"
        section_result="failed"
      fi
    else
      warn "Cannot update flake inputs (nix not found)"
      section_result="failed"
    fi

    section_end "$section_result"
    abort_failed_section "$section_result"
  fi

  #--------------------------------------------------------------------------------------------------
  # 5. Validate the result without changing the active profile
  #--------------------------------------------------------------------------------------------------

  run_validation || exit $?

  # The refreshed lock is part of the validated result; commit it even when
  # activation is deferred with --skip-home-manager.
  commit_flake_lock "$PWD"

  #--------------------------------------------------------------------------------------------------
  # 6. Optionally activate the validated configuration
  #--------------------------------------------------------------------------------------------------

  if [ "$SKIP_HOME_MANAGER" -eq 1 ]; then
    section_start "Skipping home-manager switch (--skip-home-manager)"
    section_end "skipped"
  else
    run_home_manager_switch || exit $?
  fi

  #--------------------------------------------------------------------------------------------------
  # 7. Final summary
  #--------------------------------------------------------------------------------------------------

  finish_successfully
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  main "$@"
fi
