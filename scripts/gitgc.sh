#!/usr/bin/env bash

###################################################################################################
###################################################################################################
### Git compression script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: Thu Dec 25 08:50:00 AM CST 2025
###
###################################################################################################
###################################################################################################

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: ./scripts/gitgc.sh [--aggressive] [directory]

Run Git's normal garbage collection policy in the main repository and every
initialized submodule. Reflog retention and unreachable-object expiry follow
Git's configured grace periods; this script never forces immediate expiry.

Options:
  --aggressive  Spend more time optimizing packs; retain Git's normal expiry.
  -h, --help    Show this help.
EOF
}

AGGRESSIVE=false
SEARCH_DIR="."

while [[ $# -gt 0 ]]; do
  case "$1" in
  -h | --help)
    usage
    exit 0
    ;;
  --aggressive)
    AGGRESSIVE=true
    shift
    ;;
  -*)
    printf 'Unknown option: %s\n' "$1" >&2
    usage >&2
    exit 1
    ;;
  *)
    if [[ $SEARCH_DIR != "." ]]; then
      printf 'Multiple directories specified: %s and %s
' "$SEARCH_DIR" "$1" >&2
      usage >&2
      exit 1
    fi
    SEARCH_DIR="$1"
    shift
    ;;
  esac
done

cd -- "$SEARCH_DIR"

if ! command -v git &>/dev/null; then
  printf 'Error: git not found in PATH\n' >&2
  exit 1
fi

if ! git rev-parse --git-dir &>/dev/null; then
  printf 'Error: not a git repository\n' >&2
  exit 1
fi

GC_ARGS=()
if [ "$AGGRESSIVE" = true ]; then
  GC_ARGS+=(--aggressive)
  printf '%s\n' "Running aggressive gc (this may take a while)..."
fi

printf '%s\n' "=== Pruning stale remote-tracking branches ==="
if git remote get-url origin >/dev/null 2>&1; then
  git remote prune origin ||
    printf '%s\n' "Warning: could not prune origin; continuing with local gc." >&2
else
  printf '%s\n' "No origin remote configured; skipping main-repository remote prune."
fi

printf '\n'
printf '%s\n' "=== Running git gc on main repo ==="
git gc "${GC_ARGS[@]}"

printf '\n'
printf '%s\n' "=== Processing submodules ==="
export AGGRESSIVE
# shellcheck disable=SC2016 # Expanded by git submodule foreach's shell.
git submodule foreach --recursive '
  printf "Processing: %s\n" "$sm_path"
  if git remote get-url origin >/dev/null 2>&1; then
    git remote prune origin 2>/dev/null || true
  fi
  if [ "$AGGRESSIVE" = true ]; then
    git gc --aggressive
  else
    git gc
  fi
'
export -n AGGRESSIVE

printf '\n'
printf '%s\n' "=== Disk usage summary ==="
du -sh "$(git rev-parse --git-common-dir)"
# shellcheck disable=SC2016 # Expanded by git submodule foreach's shell.
git submodule foreach --recursive 'du -sh "$(git rev-parse --git-common-dir)" 2>/dev/null || true'

printf '\n'
printf '%s\n' "Done! Git's configured reflog and object grace periods were preserved."
