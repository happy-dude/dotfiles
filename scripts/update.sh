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
# Updates git repo, submodules, vim plugins, treesitter parsers, coc.nvim, and Go binaries.
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

echo "=== Updating dotfiles repository ==="

# Pull latest changes for main repo
git pull --rebase

# Sync submodule URLs (in case .gitmodules changed)
git submodule sync --recursive

# Initialize any new submodules
git submodule update --init --recursive

# Update all submodules to latest on their configured branch (from .gitmodules)
echo
echo "=== Updating submodules to latest remote commits ==="
git submodule update --remote --recursive

# Show status
echo
echo "=== Submodule status ==="
git submodule status --recursive

#---------------------------------------
# Neovim/Vim: Update vim-plug plugins
#---------------------------------------
if command -v nvim &>/dev/null; then
    echo
    echo "=== Updating vim-plug plugins ==="
    # Use -V1 for verbose messages, redirect all output to stdout
    # PlugUpgrade and PlugUpdate with explicit message capture
    nvim -V1 --headless \
        +'PlugUpgrade' \
        +'PlugUpdate' \
        +'messages' \
        +'qall' 2>&1 || {
        echo "Warning: vim-plug update encountered an error (exit code: $?)"
    }
else
    echo
    echo "Skipping vim-plug updates (nvim not found)"
fi

#---------------------------------------
# Neovim: Update Treesitter parsers
#---------------------------------------
if command -v nvim &>/dev/null; then
    echo
    echo "=== Updating Treesitter parsers ==="
    # TSUpdate can take time; show all output
    nvim -V1 --headless \
        +'TSUpdate' \
        +'sleep 30' \
        +'messages' \
        +'qall' 2>&1 || {
        echo "Warning: TSUpdate encountered an error (exit code: $?)"
        echo "nvim-treesitter may not be installed or configured"
    }
else
    echo
    echo "Skipping Treesitter updates (nvim not found)"
fi

#---------------------------------------
# Neovim: Update coc.nvim extensions
#---------------------------------------
if command -v nvim &>/dev/null; then
    echo
    echo "=== Updating coc.nvim extensions ==="
    # Try CocUpdateSync first, fall back to CocUpdate with sleep
    if nvim -V1 --headless \
        +'CocUpdateSync' \
        +'messages' \
        +'qall' 2>&1; then
        echo "coc.nvim extensions updated (sync)"
    else
        echo "CocUpdateSync not available or failed, trying CocUpdate..."
        nvim -V1 --headless \
            +'CocUpdate' \
            +'sleep 30' \
            +'messages' \
            +'qall' 2>&1 || {
            echo "Warning: CocUpdate encountered an error (exit code: $?)"
            echo "coc.nvim may not be installed"
        }
    fi
else
    echo
    echo "Skipping coc.nvim updates (nvim not found)"
fi

#---------------------------------------
# Neovim: Update Go binaries (vim-go or go.nvim)
#---------------------------------------
if command -v nvim &>/dev/null && command -v go &>/dev/null; then
    echo
    echo "=== Updating Go binaries ==="
    # Create temp file for Go commands (some plugins need a .go file open)
    TMPGO=$(mktemp --suffix=.go)
    trap "rm -f '$TMPGO'" EXIT
    echo 'package main' > "$TMPGO"

    if nvim -V1 --headless "$TMPGO" \
        +'GoUpdateBinaries' \
        +'sleep 60' \
        +'messages' \
        +'qall' 2>&1; then
        echo "vim-go binaries updated"
    elif nvim -V1 --headless "$TMPGO" \
        +'GoInstallBinaries' \
        +'sleep 60' \
        +'messages' \
        +'qall' 2>&1; then
        echo "go.nvim binaries updated"
    else
        echo "Warning: Go binary update encountered an error (exit code: $?)"
        echo "vim-go or go.nvim may not be installed"
    fi

    rm -f "$TMPGO"
    trap - EXIT
else
    echo
    echo "Skipping Go binary updates (nvim or go not found)"
fi

echo
echo "=== Done! ==="
echo "Review changes with: git diff"
echo "Commit with: git add -A && git commit -m 'Update submodules'"
