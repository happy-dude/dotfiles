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

# Usage: ./git-gc.sh [--aggressive] [directory]
# Prunes and garbage collects the main repo and all submodules

# Parse options first
AGGRESSIVE=false
SEARCH_DIR="."

while [[ $# -gt 0 ]]; do
    case "$1" in
        --aggressive)
            AGGRESSIVE=true
            shift
            ;;
        -*)
            echo "Unknown option: $1" >&2
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

gc_opts="--prune=now"
if $AGGRESSIVE; then
    gc_opts="--aggressive --prune=now"
    echo "Running aggressive gc (this may take a while)..."
fi

echo "=== Pruning stale remote-tracking branches ==="
git remote prune origin
git reflog expire --expire=now --all

echo
echo "=== Running git gc on main repo ==="
git gc $gc_opts

echo
echo "=== Processing submodules ==="
git submodule foreach --recursive "
    echo \"Processing: \$sm_path\"
    git remote prune origin 2>/dev/null || true
    git reflog expire --expire=now --all 2>/dev/null || true
    git gc $gc_opts
"

echo
echo "=== Disk usage summary ==="
du -sh .git
git submodule foreach --recursive 'du -sh .git 2>/dev/null || true'

echo
echo "Done!"
