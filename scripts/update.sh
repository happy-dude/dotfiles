#!/usr/bin/env bash

###################################################################################################
###################################################################################################
### Update script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: Fri Mar 07 2026
###
### Workflow:
###     1. Update Rime schemas via plum
###     2. Update the main dotfiles repository
###     3. Sync / init / update git submodules
###     4. Update Neovim plugins, Treesitter parsers, and coc.nvim extensions
###     5. Update Go editor binaries
###     6. Run `nix fmt .`
###     7. Run `nix-channel --update`
###     8. Run `nix flake update`
###     9. Run `home-manager switch`
###
### Usage:
###     ./scripts/update.sh [options] [directory]
###
### Notes:
###     - Dirty top-level repo pull is allowed via:
###           git pull --rebase --autostash
###
###     - Dirty submodules are NOT updated unless either:
###           1. you clean them up yourself, or
###           2. you pass --autostash-submodules
###
###     - Set tracked submodule branches in .gitmodules with:
###           git submodule set-branch --branch <branch> <path>
###
###################################################################################################
###################################################################################################

set -euo pipefail

#--------------------------------------------------------------------------------------------------
# Defaults / globals
#--------------------------------------------------------------------------------------------------

AUTO_STASH_SUBMODULES=0
SKIP_RIME=0
SKIP_PULL=0
SKIP_SUBMODULES=0
SKIP_STATUS=0
SKIP_NVIM=0
SKIP_GO=0
SKIP_NIX_FMT=0
SKIP_NIX_CHANNEL=0
SKIP_NIX_FLAKE=0
SKIP_HOME_MANAGER=0

VERBOSE="${VERBOSE:-0}"

REPO_DIR="."
TMPGO=""
STASHED_SUBMODULES=()

SECTION_NAME=""
SECTION_START=0
SCRIPT_START_SECS=$SECONDS
SCRIPT_START_TS="$(date '+%Y-%m-%d %I:%M:%S %p %Z')"

SECTION_LABELS=()
SECTION_RESULTS=()
SECTION_TIMES=()

PLUM_DIR="${PLUM_DIR:-$HOME/plum}"
RIME_FRONTEND="${RIME_FRONTEND:-fcitx5-rime}"
# Default to the flake output matching the current user (schan, stachan, ...),
# so each machine switches its own config. Override with HOME_MANAGER_FLAKE.
HOME_MANAGER_FLAKE="${HOME_MANAGER_FLAKE:-.#$(whoami)}"

RIME_PACKAGES=(
    plum
    bopomofo
    cangjie
    essay
    luna-pinyin
    prelude
    stroke
    terra-pinyin
    cantonese
    jyutping
    CanCLID/rime-loengfan
    felixonmars/fcitx5-pinyin-zhwiki
)

#--------------------------------------------------------------------------------------------------
# Usage
#--------------------------------------------------------------------------------------------------

usage() {
    cat <<'EOF'
Usage:
  ./scripts/update.sh [options] [directory]

Options:

  General:
    --verbose
        Enable verbose script output.

    --quiet
        Reduce script output (default).

    -h, --help
        Show this help.

  Rime:
    --skip-rime
        Skip Rime schema installation/update via plum.

  Git / dotfiles:
    --skip-pull
        Skip `git pull --rebase --autostash`.

    --skip-submodules
        Skip submodule sync/init/check/update steps.

    --skip-status
        Skip `git submodule status --recursive`.

    --autostash-submodules
        Automatically stash dirty submodules before updating them.
        Stashes are NOT automatically popped afterward.

  Neovim / Go:
    --skip-nvim
        Skip vim-plug, Treesitter, and coc.nvim updates.

    --skip-go
        Skip Go binary updates.

  Nix / Home Manager:
    --skip-nix-fmt
        Skip `nix fmt .`.

    --skip-nix-channel
        Skip `nix-channel --update`.

    --skip-nix-flake
        Skip `nix flake update`.

    --skip-home-manager
        Skip `home-manager switch`.

Environment:
  VERBOSE=1
      Enable verbose output.

  VERBOSE=0
      Quiet output (default).

  PLUM_DIR=...
      Path to plum checkout. Default: ~/plum

  RIME_FRONTEND=...
      Rime frontend passed to rime-install. Default: fcitx5-rime

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

warn() {
    printf 'Warning: %s\n' "$*" >&2
}

die() {
    printf 'Error: %s\n' "$*" >&2
    exit 1
}

cleanup() {
    if [ -n "${TMPGO:-}" ]; then
        rm -f -- "$TMPGO"
    fi
}

trap cleanup EXIT

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
    printf '  %-14s %s\n' "Review:" "git diff"
    printf '  %-14s %s\n' "Commit:" "git add -A && git commit -m 'Update submodules'"

    if [ "${#STASHED_SUBMODULES[@]}" -gt 0 ]; then
        printf '  %-14s %s\n' "Auto-stashed:" "${#STASHED_SUBMODULES[@]} submodule(s)"
        for i in "${!STASHED_SUBMODULES[@]}"; do
            printf '  %-14s %s\n' "" "${STASHED_SUBMODULES[$i]}"
        done
        printf '  %-14s %s\n' "Stash review:" "git -C <submodule-path> stash list"
    fi
}

create_temp_go_file() {
    local tmpbase

    if tmpbase="$(mktemp "${TMPDIR:-/tmp}/update-go.XXXXXX" 2>/dev/null)"; then
        :
    else
        tmpbase="$(TMPDIR="${TMPDIR:-/tmp}" mktemp -t update-go 2>/dev/null)" || return 1
    fi

    TMPGO="${tmpbase}.go"

    if mv -- "$tmpbase" "$TMPGO" 2>/dev/null; then
        :
    else
        cp -- "$tmpbase" "$TMPGO"
        rm -f -- "$tmpbase"
    fi

    printf 'package main\n\nfunc main() {}\n' > "$TMPGO"
}

#--------------------------------------------------------------------------------------------------
# 1. Rime / plum helpers
#--------------------------------------------------------------------------------------------------

run_rime_install() {
    (
        cd "$PLUM_DIR"
        env rime_frontend="$RIME_FRONTEND" bash ./rime-install "${RIME_PACKAGES[@]}"
    )
}

force_refresh_git_tags_in_dir() {
    local dir="$1"
    local gitmeta repo

    [ -d "$dir" ] || return 0

    while IFS= read -r -d '' gitmeta; do
        repo="${gitmeta%/.git}"
        [ -n "$repo" ] || continue

        vmsg "Force-refreshing tags in: $repo"
        git -C "$repo" fetch --tags --force || true
        git -C "$repo" fetch --force origin || true
    done < <(find "$dir" \( -type d -name .git -o -type f -name .git \) -print0)
}

recover_rime_git_state() {
    msg "Rime update failed; trying forced Git tag refresh and one retry..."
    force_refresh_git_tags_in_dir "$PLUM_DIR"
}

#--------------------------------------------------------------------------------------------------
# 2. Git / submodule helpers
#--------------------------------------------------------------------------------------------------

is_submodule_dirty() {
    local path="$1"
    [ -n "$(git -C "$path" status --porcelain --untracked-files=normal)" ]
}

collect_dirty_submodules() {
    local path
    while IFS= read -r path; do
        [ -n "$path" ] || continue
        if is_submodule_dirty "$path"; then
            printf '%s\n' "$path"
        fi
    done < <(git submodule foreach --quiet --recursive 'printf "%s\n" "$displaypath"')
}

stash_dirty_submodules() {
    local path
    while IFS= read -r path; do
        [ -n "$path" ] || continue
        if is_submodule_dirty "$path"; then
            msg "Stashing dirty submodule: $path"
            git -C "$path" stash push -u -m "update.sh auto-stash"
            STASHED_SUBMODULES+=("$path")
        fi
    done < <(git submodule foreach --quiet --recursive 'printf "%s\n" "$displaypath"')
}

#--------------------------------------------------------------------------------------------------
# 3. Neovim helpers
#--------------------------------------------------------------------------------------------------

run_nvim_cmd_if_exists() {
    local cmd_name="$1"
    local ex_cmd="$2"
    shift 2

    local escaped
    escaped=${ex_cmd//\'/\'\'}

    if [ "${VERBOSE:-0}" -eq 1 ]; then
        nvim -V1 --headless "$@" \
            -c "if exists(':${cmd_name}') | execute '${escaped}' | else | cquit 3 | endif" \
            -c "messages" \
            -c "qall" 2>&1
    else
        nvim --headless "$@" \
            -c "if exists(':${cmd_name}') | execute '${escaped}' | else | cquit 3 | endif" \
            -c "qall" >/dev/null 2>&1
    fi
}

#--------------------------------------------------------------------------------------------------
# Argument parsing
#--------------------------------------------------------------------------------------------------

while [ $# -gt 0 ]; do
    case "$1" in
        --verbose)
            VERBOSE=1
            ;;
        --quiet)
            VERBOSE=0
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        --skip-rime)
            SKIP_RIME=1
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
        --skip-nvim)
            SKIP_NVIM=1
            ;;
        --skip-go)
            SKIP_GO=1
            ;;
        --skip-nix-fmt)
            SKIP_NIX_FMT=1
            ;;
        --skip-nix-channel)
            SKIP_NIX_CHANNEL=1
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
            REPO_DIR="$1"
            ;;
    esac
    shift
done

#--------------------------------------------------------------------------------------------------
# Basic checks
#--------------------------------------------------------------------------------------------------

cd "$REPO_DIR"

if ! have git; then
    die "git not found in PATH"
fi

if ! git rev-parse --git-dir >/dev/null 2>&1; then
    die "not a git repository"
fi

msg "=== Updating dotfiles repository ==="
msg "Started at: $SCRIPT_START_TS"

#--------------------------------------------------------------------------------------------------
# 1. Updating Rime schemas
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_RIME" -eq 1 ]; then
    section_start "Skipping Rime update (--skip-rime)"
    section_end "skipped"
else
    section_start "Updating Rime schemas"
    section_result="done"

    if [ ! -d "$PLUM_DIR" ]; then
        warn "Skipping Rime update ($PLUM_DIR not found)"
        section_result="skipped"
    elif [ ! -f "$PLUM_DIR/rime-install" ]; then
        warn "Skipping Rime update ($PLUM_DIR/rime-install not found)"
        section_result="skipped"
    else
        rime_status=0
        run_rime_install || rime_status=$?

        if [ "$rime_status" -ne 0 ]; then
            recover_rime_git_state
            rime_status=0
            run_rime_install || rime_status=$?
        fi

        if [ "$rime_status" -ne 0 ]; then
            warn "Rime update still failed after forced tag refresh (exit code: $rime_status)"
            section_result="failed"
        fi
    fi

    section_end "$section_result"
fi

#--------------------------------------------------------------------------------------------------
# 2. Updating the main dotfiles repository
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
# 3. Sync / init / update git submodules
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_SUBMODULES" -eq 0 ]; then
    section_start "Syncing submodule URLs"
    git submodule sync --recursive
    section_end "done"

    section_start "Initializing submodules"
    git submodule update --init --recursive
    section_end "done"

    section_start "Checking submodules for local changes"

    mapfile -t DIRTY_SUBMODULES < <(collect_dirty_submodules)

    if [ "${#DIRTY_SUBMODULES[@]}" -gt 0 ]; then
        if [ "$AUTO_STASH_SUBMODULES" -eq 1 ]; then
            msg "Dirty submodules detected; auto-stashing them now..."
            stash_dirty_submodules
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

    section_start "Updating submodules to latest remote commits"
    git submodule update --remote --recursive
    section_end "done"
else
    section_start "Skipping submodule operations (--skip-submodules)"
    section_end "skipped"
fi

if [ "$SKIP_STATUS" -eq 1 ]; then
    section_start "Skipping submodule status (--skip-status)"
    section_end "skipped"
else
    section_start "Submodule status"
    git submodule status --recursive
    section_end "done"
fi

#--------------------------------------------------------------------------------------------------
# 4. Updating Neovim plugins, Treesitter parsers, and coc.nvim extensions
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_NVIM" -eq 1 ]; then
    section_start "Skipping Neovim/Vim updates (--skip-nvim)"
    section_end "skipped"
elif have nvim; then
    section_start "Updating vim-plug plugins"

    status=0
    section_result="done"
    run_nvim_cmd_if_exists "PlugUpdate" "silent! PlugUpgrade | PlugUpdate --sync" || status=$?

    case "$status" in
        0)
            vmsg "vim-plug update completed"
            ;;
        3)
            msg "Skipping vim-plug updates (PlugUpdate not available in this headless nvim session)"
            section_result="skipped"
            ;;
        *)
            warn "vim-plug update encountered an error (exit code: $status)"
            section_result="failed"
            ;;
    esac

    section_end "$section_result"

    section_start "Updating Treesitter parsers"

    status=0
    section_result="done"
    run_nvim_cmd_if_exists "TSUpdate" "TSUpdate" || status=$?

    case "$status" in
        0)
            vmsg "Treesitter parsers updated"
            ;;
        3)
            msg "Skipping Treesitter updates (TSUpdate not available in this headless nvim session)"
            section_result="skipped"
            ;;
        *)
            warn "Treesitter update encountered an error (exit code: $status)"
            warn "nvim-treesitter may not be installed or configured"
            section_result="failed"
            ;;
    esac

    section_end "$section_result"

    section_start "Updating coc.nvim extensions"

    status=0
    section_result="done"
    run_nvim_cmd_if_exists "CocUpdateSync" "CocUpdateSync" || status=$?

    case "$status" in
        0)
            vmsg "coc.nvim extensions updated"
            ;;
        3)
            msg "Skipping coc.nvim updates (CocUpdateSync not available in this headless nvim session)"
            section_result="skipped"
            ;;
        *)
            warn "CocUpdateSync encountered an error (exit code: $status)"
            warn "coc.nvim may not be installed"
            section_result="failed"
            ;;
    esac

    section_end "$section_result"
else
    section_start "Skipping Neovim/Vim updates (nvim not found)"
    section_end "skipped"
fi

#--------------------------------------------------------------------------------------------------
# 5. Updating Go editor binaries
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_GO" -eq 1 ]; then
    section_start "Skipping Go binary updates (--skip-go)"
    section_end "skipped"
elif [ "$SKIP_NVIM" -eq 1 ]; then
    section_start "Skipping Go binary updates (--skip-nvim)"
    section_end "skipped"
elif have nvim && have go; then
    section_start "Updating Go binaries"

    section_result="done"

    if ! create_temp_go_file; then
        warn "Unable to create temporary Go file"
        section_result="failed"
    else
        status=0
        run_nvim_cmd_if_exists "GoUpdateBinaries" "GoUpdateBinaries" "$TMPGO" || status=$?

        if [ "$status" -eq 0 ]; then
            vmsg "vim-go binaries updated"
        else
            vmsg "GoUpdateBinaries not available or failed, trying GoInstallBinaries..."
            status=0
            run_nvim_cmd_if_exists "GoInstallBinaries" "GoInstallBinaries" "$TMPGO" || status=$?

            case "$status" in
                0)
                    vmsg "Go binaries updated with GoInstallBinaries"
                    ;;
                3)
                    msg "Skipping Go binary updates (no supported Go update command found in this headless nvim session)"
                    section_result="skipped"
                    ;;
                *)
                    warn "Go binary update encountered an error (exit code: $status)"
                    warn "vim-go or go.nvim may not be installed"
                    section_result="failed"
                    ;;
            esac
        fi
    fi

    section_end "$section_result"
else
    section_start "Skipping Go binary updates (nvim or go not found)"
    section_end "skipped"
fi

#--------------------------------------------------------------------------------------------------
# 6. Run `nix fmt .`
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_NIX_FMT" -eq 1 ]; then
    section_start "Skipping nix fmt (--skip-nix-fmt)"
    section_end "skipped"
else
    section_start "Formatting flake with nix fmt"

    if have nix; then
        status=0
        nix fmt . || status=$?

        if [ "$status" -eq 0 ]; then
            section_end "done"
        else
            warn "nix fmt failed (exit code: $status)"
            section_end "failed"
        fi
    else
        warn "Skipping nix fmt (nix not found)"
        section_end "skipped"
    fi
fi

#--------------------------------------------------------------------------------------------------
# 7. Run `nix-channel --update`
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_NIX_CHANNEL" -eq 1 ]; then
    section_start "Skipping nix-channel update (--skip-nix-channel)"
    section_end "skipped"
else
    section_start "Updating nix channels"

    if have nix-channel; then
        status=0
        nix-channel --update || status=$?

        if [ "$status" -eq 0 ]; then
            section_end "done"
        else
            warn "nix-channel --update failed (exit code: $status)"
            section_end "failed"
        fi
    else
        warn "Skipping nix-channel update (nix-channel not found)"
        section_end "skipped"
    fi
fi

#--------------------------------------------------------------------------------------------------
# 8. Run `nix flake update`
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_NIX_FLAKE" -eq 1 ]; then
    section_start "Skipping nix flake update (--skip-nix-flake)"
    section_end "skipped"
else
    section_start "Updating flake inputs"

    if have nix; then
        status=0
        nix flake update || status=$?

        if [ "$status" -eq 0 ]; then
            section_end "done"
        else
            warn "nix flake update failed (exit code: $status)"
            section_end "failed"
        fi
    else
        warn "Skipping nix flake update (nix not found)"
        section_end "skipped"
    fi
fi

#--------------------------------------------------------------------------------------------------
# 9. Run `home-manager switch`
#--------------------------------------------------------------------------------------------------

if [ "$SKIP_HOME_MANAGER" -eq 1 ]; then
    section_start "Skipping home-manager switch (--skip-home-manager)"
    section_end "skipped"
else
    section_start "Running home-manager switch"

    if have home-manager; then
        status=0
        home-manager switch --flake "$HOME_MANAGER_FLAKE" --show-trace || status=$?

        if [ "$status" -eq 0 ]; then
            section_end "done"
        else
            warn "home-manager switch failed (exit code: $status)"
            section_end "failed"
        fi
    else
        warn "Skipping home-manager switch (home-manager not found)"
        section_end "skipped"
    fi
fi

#--------------------------------------------------------------------------------------------------
# 10. Final summary
#--------------------------------------------------------------------------------------------------

SCRIPT_END_TS="$(date '+%Y-%m-%d %I:%M:%S %p %Z')"
TOTAL_ELAPSED=$((SECONDS - SCRIPT_START_SECS))

msg
msg "=== Done! ==="

print_final_summary "$SCRIPT_END_TS" "$TOTAL_ELAPSED"
print_timing_summary
