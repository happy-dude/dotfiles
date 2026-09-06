#!/usr/bin/env bash
set -euo pipefail

# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/report.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/report.sh"
# shellcheck source-path=SCRIPTDIR
# shellcheck source=lib/git-worktree.sh
source "$(dirname "${BASH_SOURCE[0]}")/lib/git-worktree.sh"
IFS=$'\n\t'

repo_root=$(git rev-parse --show-toplevel)
readonly repo_root

usage() {
  cat <<'EOF'
Usage:
  portable-series.sh start <name> [worktree]
  portable-series.sh export <name> [output-directory]
  portable-series.sh clean <name>

start creates replay/<name> in an isolated worktree rooted at origin/main.
export validates that branch and writes patch artifacts without pushing.
clean removes the worktree and branch of a series whose commits are all
represented in origin/main.
EOF
}

validate_name() {
  [[ $1 =~ ^[a-z0-9][a-z0-9._-]*$ ]] ||
    die "invalid series name: $1"
}

reject_merge_commits() {
  local worktree=$1
  local base=$2
  local merge_commits

  merge_commits=$(
    git -C "$worktree" rev-list --min-parents=2 "$base..HEAD"
  )
  [[ -z $merge_commits ]] || die "portable history contains merge commits"
}

validate_forbidden_pattern() {
  local pattern=$1
  local status

  [[ -n $pattern ]] || return 0
  if grep -Eq -- "$pattern" /dev/null; then
    return 0
  else
    status=$?
  fi
  ((status == 1)) || die "invalid portable forbidden-content pattern"
}

scan_forbidden_content() {
  local pattern=$1
  local category=$2
  local input_path=$3
  local matches
  local status

  [[ -n $pattern ]] || return 0
  if matches=$(grep -Ein -- "$pattern" "$input_path" | cut -d: -f 1); then
    printf 'error: forbidden content found in %s at line(s): %s\n' \
      "$category" "$(printf '%s\n' "$matches" | paste -sd, -)" >&2
    return 1
  else
    status=$?
  fi
  if ((status != 1)); then
    die "failed to scan portable $category"
  fi
}

write_scan_inputs() {
  local worktree=$1
  local base=$2
  local metadata_path=$3
  local content_path=$4
  local commit

  : >"$metadata_path"
  while IFS= read -r commit; do
    git -C "$worktree" show -s --format=%B "$commit" |
      git interpret-trailers \
        --if-exists replace \
        --if-missing doNothing \
        --trailer 'Assisted-by:' >>"$metadata_path"
    printf '\n' >>"$metadata_path"
  done < <(git -C "$worktree" rev-list --reverse "$base..HEAD")
  git -C "$worktree" diff --binary --full-index --unified=0 \
    "$base..HEAD" >"$content_path"
}

scan_series() {
  local pattern=$1
  local worktree=$2
  local base=$3
  local staging_directory=$4
  local metadata_path="$staging_directory/commit-metadata.txt"
  local content_path="$staging_directory/changed-content.diff"

  [[ -n $pattern ]] || return 0
  write_scan_inputs "$worktree" "$base" "$metadata_path" "$content_path"
  scan_forbidden_content "$pattern" "commit metadata" "$metadata_path"
  scan_forbidden_content "$pattern" "changed content" "$content_path"
}

scan_final_patch() {
  local pattern=$1
  local patch_path=$2
  local staging_directory=$3
  local scan_path="$staging_directory/final-patch.txt"

  [[ -n $pattern ]] || return 0
  sed '/^Assisted-by:/d' "$patch_path" >"$scan_path"
  scan_forbidden_content "$pattern" "final patch" "$scan_path"
}

create_bundle() {
  local staging_directory=$1
  local bundle_directory=$2
  local bundle_name=$3
  local bundle_checksum_name=$4
  shift 4
  local artifact_name

  mkdir "$staging_directory/$bundle_directory"
  for artifact_name in "$@"; do
    if [[ ! -f $staging_directory/$artifact_name ]]; then
      die "staged artifact not found: $artifact_name"
      return 1
    fi
    cp -- "$staging_directory/$artifact_name" \
      "$staging_directory/$bundle_directory/"
  done
  tar -C "$staging_directory" -czf "$staging_directory/$bundle_name" \
    "$bundle_directory"
  (
    cd -- "$staging_directory"
    sha256sum "$bundle_name" >"$bundle_checksum_name"
  )
}

publish_artifacts() {
  local staging_directory=$1
  local output_directory=$2
  local checksum_name=$3
  local bundle_checksum_name=$4
  shift 4
  local artifact_name

  for artifact_name in "$@" "$checksum_name" "$bundle_checksum_name"; do
    if [[ ! -f $staging_directory/$artifact_name ]]; then
      die "staged artifact not found: $artifact_name"
      return 1
    fi
  done

  rm -f -- \
    "$output_directory/$checksum_name" \
    "$output_directory/$bundle_checksum_name"
  for artifact_name in "$@"; do
    mv -f -- "$staging_directory/$artifact_name" "$output_directory/"
  done
  mv -f -- "$staging_directory/$checksum_name" "$output_directory/"
  mv -f -- \
    "$staging_directory/$bundle_checksum_name" "$output_directory/"
}

apply_artifact_name() {
  printf 'apply-dotfiles-%s.sh\n' "$1"
}

print_existing_series_help() {
  local name=$1
  local worktree=$2

  printf '%s\n' \
    "Continue the existing series in: $worktree" \
    "Export it when ready with:" >&2
  printf '  %q export %q\n' \
    "$repo_root/scripts/portable-series.sh" "$name" >&2
}

print_worktree_inspection_help() {
  printf '%s\n' "Inspect worktree registrations with:" >&2
  printf '  git -C %q worktree list --porcelain\n' "$repo_root" >&2
}

start_series() {
  local name=$1
  local worktree=${2:-"/tmp/dotfiles-portable-$name"}
  local branch="replay/$name"
  local branch_ref="refs/heads/$branch"
  local existing_worktree
  local lookup_status

  validate_name "$name"
  if git -C "$repo_root" show-ref --verify --quiet "$branch_ref"; then
    if existing_worktree=$(worktree_for_branch "$repo_root" "$branch_ref"); then
      print_existing_series_help "$name" "$existing_worktree"
      die "branch already exists with worktree: $branch ($existing_worktree)"
    else
      lookup_status=$?
      if ((lookup_status == 2)); then
        print_worktree_inspection_help
        die "branch already exists with prunable worktree registration: $branch"
      else
        printf '%s\n' "Attach the existing branch to a reviewed path with:" >&2
        printf '  git -C %q worktree add <path> %q\n' \
          "$repo_root" "$branch" >&2
        die "branch already exists without a worktree: $branch"
      fi
    fi
    return 1
  fi
  if [[ -e $worktree || -L $worktree ]]; then
    die "worktree path already exists: $worktree"
    return 1
  fi

  git -C "$repo_root" fetch origin main
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
  # EXIT traps run after function scope ends; bake the path in.
  # shellcheck disable=SC2064 # Early expansion is the point: EXIT runs after function scope ends.
  trap "rm -rf -- $(printf '%q' "$temporary_directory")" EXIT
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
  trap - EXIT
}

export_series() {
  local name=$1
  local output_directory=${2:-"$HOME/Downloads"}
  local branch="replay/$name"
  local branch_ref="refs/heads/$branch"
  local worktree
  local lookup_status
  local base
  local merge_base
  local count
  local patch_name="dotfiles-$name.patch"
  local manifest_name="dotfiles-$name.manifest"
  local checksum_name="dotfiles-$name.sha256"
  local bundle_name="dotfiles-$name.tar.gz"
  local bundle_checksum_name="$bundle_name.sha256"
  local apply_name
  local staging_directory
  local staged_patch_path
  local staged_manifest_path
  local staged_apply_path
  local bundle_directory="dotfiles-$name"
  local patch_sha256
  local forbidden_pattern=${PORTABLE_FORBIDDEN_PATTERN:-}

  validate_name "$name"
  apply_name=$(apply_artifact_name "$name")
  if worktree=$(worktree_for_branch "$repo_root" "$branch_ref"); then
    :
  else
    lookup_status=$?
    if ((lookup_status == 2)); then
      die "worktree registration is prunable for branch: $branch"
    else
      die "no worktree found for $branch"
    fi
    return 1
  fi
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
  reject_merge_commits "$worktree" "$base"

  local authors
  authors=$(git -C "$worktree" log --format='%an <%ae>' "$base..HEAD")
  if grep -Ev '^Portable Dotfiles <portable@localhost>$' <<<"$authors"; then
    die "portable history contains a non-portable author identity"
  fi
  lint_commits "$worktree" "$base"

  validate_forbidden_pattern "$forbidden_pattern"
  # lint_commits clears the process-wide traps when it returns; the staging
  # traps must be installed after it runs.
  staging_directory=$(mktemp -d "$output_directory/.dotfiles-$name.XXXXXX")
  trap 'rm -rf -- "$staging_directory"' RETURN
  # shellcheck disable=SC2064 # Early expansion is the point: EXIT runs after function scope ends.
  trap "rm -rf -- $(printf '%q' "$staging_directory")" EXIT
  scan_series "$forbidden_pattern" "$worktree" "$base" "$staging_directory"

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

  staged_patch_path="$staging_directory/$patch_name"
  staged_manifest_path="$staging_directory/$manifest_name"
  staged_apply_path="$staging_directory/$apply_name"
  git -C "$worktree" format-patch --stdout --base="$base" "$base..HEAD" \
    >"$staged_patch_path"
  scan_final_patch \
    "$forbidden_pattern" "$staged_patch_path" "$staging_directory"
  rm -f -- \
    "$staging_directory/commit-metadata.txt" \
    "$staging_directory/changed-content.diff" \
    "$staging_directory/final-patch.txt"
  patch_sha256=$(sha256sum "$staged_patch_path" | cut -d ' ' -f 1)

  cat >"$staged_manifest_path" <<EOF
version=1
name=$name
base=$base
count=$count
patch=$patch_name
sha256=$patch_sha256
EOF
  cp -- "$repo_root/scripts/apply-portable-series.sh" \
    "$staged_apply_path"
  chmod 0755 "$staged_apply_path"
  (
    cd -- "$staging_directory"
    sha256sum "$patch_name" "$manifest_name" "$apply_name" \
      >"$checksum_name"
  )

  create_bundle \
    "$staging_directory" \
    "$bundle_directory" \
    "$bundle_name" \
    "$bundle_checksum_name" \
    "$patch_name" \
    "$manifest_name" \
    "$checksum_name" \
    "$apply_name"
  publish_artifacts \
    "$staging_directory" \
    "$output_directory" \
    "$checksum_name" \
    "$bundle_checksum_name" \
    "$patch_name" \
    "$manifest_name" \
    "$apply_name" \
    "$bundle_name"
  rm -rf -- "$staging_directory"
  trap - RETURN
  trap - EXIT

  printf '%s\n' \
    "Portable artifacts written to $output_directory" \
    "$patch_name" "$manifest_name" "$checksum_name" "$apply_name" \
    "$bundle_name" "$bundle_checksum_name" \
    "Nothing was pushed. Transfer, review, and apply them on the destination computer."
}

clean_series() {
  local name=$1
  local branch="replay/$name"
  local branch_ref="refs/heads/$branch"
  local worktree
  local lookup_status

  validate_name "$name" || return 1
  git -C "$repo_root" show-ref --verify --quiet "$branch_ref" || {
    die "no such series: $branch"
    return 1
  }
  git -C "$repo_root" show-ref --verify --quiet refs/remotes/origin/main || {
    die "origin/main is not fetched; run git fetch origin main first"
    return 1
  }

  local unrepresented
  unrepresented=$(
    git -C "$repo_root" cherry refs/remotes/origin/main "$branch_ref"
  ) || {
    die "could not compare $branch against origin/main"
    return 1
  }
  if grep -q '^+' <<<"$unrepresented"; then
    die "$branch has commits not represented in origin/main; export and apply them, or git fetch origin main first"
    return 1
  fi

  if worktree=$(worktree_for_branch "$repo_root" "$branch_ref"); then
    if [[ $worktree == "$repo_root" ]]; then
      die "run clean from the main checkout, not from the series worktree $worktree"
      return 1
    fi
    if [[ -n $(
      git -C "$worktree" status --porcelain=v1 --untracked-files=all
    ) ]]; then
      die "series worktree is not clean: $worktree"
      return 1
    fi
    git -C "$repo_root" worktree remove "$worktree"
  else
    lookup_status=$?
    if ((lookup_status == 2)); then
      print_worktree_inspection_help
      die "prunable worktree registration for $branch; inspect before cleaning"
      return 1
    fi
  fi
  git -C "$repo_root" branch -D "$branch"

  printf 'Removed series %s; nothing was pushed.\n' "$branch"
}

main() {
  local command_name=${1:-}

  case $command_name in
  start)
    (($# >= 2 && $# <= 3)) || {
      usage
      return 2
    }
    start_series "$2" "${3:-}"
    ;;
  export)
    (($# >= 2 && $# <= 3)) || {
      usage
      return 2
    }
    export_series "$2" "${3:-}"
    ;;
  clean)
    (($# == 2)) || {
      usage
      return 2
    }
    clean_series "$2"
    ;;
  *)
    usage
    return 2
    ;;
  esac
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
  main "$@"
fi
