#!/usr/bin/env bash

###################################################################################################
###################################################################################################
### Git repository update script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: Thu Dec 25 08:39:16 AM CST 2025
###
### Based on information from:
###	Christophe Portneuve:	https://medium.com/@porteneuve/mastering-git-submodules-34c65e940407
###
###################################################################################################
###################################################################################################

set -euo pipefail

# Usage: ./update-submodules.sh [directory]
# Updates all git submodules to latest commit on their configured branch.
# Set branches in .gitmodules with: git submodule set-branch --branch <branch> <path>

cd "${1:-.}"

if ! command -v git &>/dev/null; then
    echo "Error: git not found in PATH" >&2
    exit 1
fi

if ! git rev-parse --git-dir &>/dev/null; then
    echo "Error: not a git repository" >&2
    exit 1
fi

echo "Updating dotfiles repository..."

# Pull latest changes for main repo
git pull --rebase

# Sync submodule URLs (in case .gitmodules changed)
git submodule sync --recursive

# Initialize any new submodules
git submodule update --init --recursive

# Update all submodules to latest on their configured branch (from .gitmodules)
# This respects the 'branch' setting in .gitmodules for each submodule
echo
echo "Updating submodules to latest remote commits..."
git submodule update --remote --recursive

# Show status
echo
echo "Submodule status:"
git submodule status --recursive

echo
echo "Done!"
echo "Review changes with: git diff"
echo "Commit with: git add -A && git commit -m 'Update submodules'"
