#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

repo_root=$(git rev-parse --show-toplevel)
readonly repo_root

die() {
  printf 'error: %s\n' "$*" >&2
  return 1
}

usage() {
  cat <<'EOF'
Usage:
  portable-series.sh start <name> [worktree]
  portable-series.sh export <name> [output-directory]

start creates replay/<name> in an isolated worktree rooted at origin/main.
export validates that branch and writes patch artifacts without pushing.
EOF
}

validate_name() {
  [[ $1 =~ ^[a-z0-9][a-z0-9._-]*$ ]] ||
    die "invalid series name: $1"
}

worktree_for_branch() {
  local wanted_ref=$1
  local current_worktree=
  local current_branch=

  while IFS= read -r line; do
    case $line in
    "worktree "*) current_worktree=${line#worktree } ;;
    "branch "*) current_branch=${line#branch } ;;
    "")
      if [[ $current_branch == "$wanted_ref" ]]; then
        printf '%s\n' "$current_worktree"
        return 0
      fi
      current_worktree=
      current_branch=
      ;;
    esac
  done < <(
    git -C "$repo_root" worktree list --porcelain
    printf '\n'
  )
  return 1
}

start_series() {
  local name=$1
  local worktree=${2:-"/tmp/dotfiles-portable-$name"}
  local branch="replay/$name"

  validate_name "$name"
  git -C "$repo_root" fetch origin main
  if git -C "$repo_root" show-ref --verify --quiet "refs/heads/$branch"; then
    die "branch already exists: $branch"
  fi
  [[ ! -e $worktree ]] || die "worktree path already exists: $worktree"

  git -C "$repo_root" worktree add -b "$branch" "$worktree" origin/main
  git -C "$repo_root" config extensions.worktreeConfig true
  git -C "$worktree" config --worktree user.name "Portable Dotfiles"
  git -C "$worktree" config --worktree user.email "portable@localhost"
  git -C "$worktree" config --worktree commit.gpgsign false
  git -C "$worktree" config --worktree tag.gpgsign false

  printf '%s\n' \
    "Portable worktree ready: $worktree" \
    "Branch: $branch" \
    "Nothing was pushed. Develop and commit only portable changes there."
}

lint_commits() {
  local worktree=$1
  local base=$2
  local temporary_directory
  local index=0
  local commit

  temporary_directory=$(mktemp -d)
  trap 'rm -rf -- "$temporary_directory"' RETURN
  while IFS= read -r commit; do
    git -C "$worktree" show -s --format=%B "$commit" |
      python3 -c \
        'import sys; print(sys.stdin.read().rstrip() + "\n", end="")' \
        >"$temporary_directory/$(printf '%03d' "$index").md"
    python3 "$repo_root/scripts/lint_commit_message.py" \
      "$temporary_directory/$(printf '%03d' "$index").md"
    index=$((index + 1))
  done < <(git -C "$worktree" rev-list --reverse "$base..HEAD")
  rm -rf -- "$temporary_directory"
  trap - RETURN
}

export_series() {
  local name=$1
  local output_directory=${2:-"$HOME/Downloads"}
  local branch="replay/$name"
  local branch_ref="refs/heads/$branch"
  local worktree
  local base
  local merge_base
  local count
  local patch_name="dotfiles-$name.patch"
  local manifest_name="dotfiles-$name.manifest"
  local checksum_name="dotfiles-$name.sha256"
  local patch_path="$output_directory/$patch_name"
  local manifest_path="$output_directory/$manifest_name"
  local apply_name="apply-portable-series.sh"
  local patch_sha256

  validate_name "$name"
  worktree=$(worktree_for_branch "$branch_ref") ||
    die "no worktree found for $branch"
  [[ -d $output_directory ]] || die "output directory not found: $output_directory"
  [[ -z $(git -C "$worktree" status --porcelain=v1 --untracked-files=all) ]] ||
    die "portable worktree is not clean"

  git -C "$repo_root" fetch origin main
  base=$(git -C "$repo_root" rev-parse origin/main)
  merge_base=$(git -C "$worktree" merge-base HEAD origin/main)
  [[ $merge_base == "$base" ]] ||
    die "portable branch is not based on current origin/main"
  count=$(git -C "$worktree" rev-list --count "$base..HEAD")
  ((count > 0)) || die "portable branch contains no commits"

  if git -C "$worktree" log --format='%an <%ae>' "$base..HEAD" |
    grep -Ev '^Portable Dotfiles <portable@localhost>$'; then
    die "portable history contains a non-portable author identity"
  fi
  lint_commits "$worktree" "$base"

  (
    cd -- "$worktree"
    nix fmt .
    git diff --exit-code
    git diff --cached --exit-code
    nix flake check --show-trace --no-update-lock-file
    home-manager build --flake .#stachan --show-trace \
      --no-out-link --no-update-lock-file
    home-manager build --flake .#schan --show-trace \
      --no-out-link --no-update-lock-file
  )

  git -C "$worktree" format-patch --stdout --base="$base" "$base..HEAD" \
    >"$patch_path"
  if [[ -n ${PORTABLE_FORBIDDEN_PATTERN:-} ]] &&
    grep -Eiq -- "$PORTABLE_FORBIDDEN_PATTERN" "$patch_path"; then
    rm -f -- "$patch_path"
    die "forbidden content found in portable patch"
  fi
  patch_sha256=$(sha256sum "$patch_path" | cut -d ' ' -f 1)

  cat >"$manifest_path" <<EOF
version=1
name=$name
base=$base
count=$count
patch=$patch_name
sha256=$patch_sha256
EOF
  cp -- "$repo_root/scripts/apply-portable-series.sh" \
    "$output_directory/$apply_name"
  chmod 0755 "$output_directory/$apply_name"
  (
    cd -- "$output_directory"
    sha256sum "$patch_name" "$manifest_name" "$apply_name" \
      >"$checksum_name"
  )

  printf '%s\n' \
    "Portable artifacts written to $output_directory" \
    "$patch_name" "$manifest_name" "$checksum_name" "$apply_name" \
    "Nothing was pushed. Transfer, review, and apply them on the destination computer."
}

command_name=${1:-}
case $command_name in
start)
  (($# >= 2 && $# <= 3)) || {
    usage
    exit 2
  }
  start_series "$2" "${3:-}"
  ;;
export)
  (($# >= 2 && $# <= 3)) || {
    usage
    exit 2
  }
  export_series "$2" "${3:-}"
  ;;
*)
  usage
  exit 2
  ;;
esac
