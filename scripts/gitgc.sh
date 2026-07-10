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
    SEARCH_DIR="$1"
    shift
    ;;
  esac
done

cd "$SEARCH_DIR"

if ! command -v git &>/dev/null; then
  echo "Error: git not found in PATH" >&2
  exit 1
fi

if ! git rev-parse --git-dir &>/dev/null; then
  echo "Error: not a git repository" >&2
  exit 1
fi

GC_ARGS=()
if [ "$AGGRESSIVE" = true ]; then
  GC_ARGS+=(--aggressive)
  echo "Running aggressive gc (this may take a while)..."
fi

echo "=== Pruning stale remote-tracking branches ==="
if git remote get-url origin >/dev/null 2>&1; then
  git remote prune origin
else
  echo "No origin remote configured; skipping main-repository remote prune."
fi

echo
echo "=== Running git gc on main repo ==="
git gc "${GC_ARGS[@]}"

echo
echo "=== Processing submodules ==="
if [ "$AGGRESSIVE" = true ]; then
  # shellcheck disable=SC2016 # Expanded by git submodule foreach's shell.
  git submodule foreach --recursive '
    echo "Processing: $sm_path"
    if git remote get-url origin >/dev/null 2>&1; then
      git remote prune origin 2>/dev/null || true
    fi
    git gc --aggressive
  '
else
  # shellcheck disable=SC2016 # Expanded by git submodule foreach's shell.
  git submodule foreach --recursive '
    echo "Processing: $sm_path"
    if git remote get-url origin >/dev/null 2>&1; then
      git remote prune origin 2>/dev/null || true
    fi
    git gc
  '
fi

echo
echo "=== Disk usage summary ==="
du -sh .git
git submodule foreach --recursive 'du -sh .git 2>/dev/null || true'

echo
echo "Done! Git's configured reflog and object grace periods were preserved."
