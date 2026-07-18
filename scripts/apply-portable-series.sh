#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

die() {
  printf 'error: %s\n' "$*" >&2
  return 1
}

usage() {
  printf 'Usage: %s <manifest> [repository]\n' "${0##*/}" >&2
}

read_manifest() {
  local manifest=$1
  local key
  local value

  while IFS='=' read -r key value; do
    case $key in
    version) manifest_version=$value ;;
    name) series_name=$value ;;
    base) expected_base=$value ;;
    count) expected_count=$value ;;
    patch) patch_name=$value ;;
    sha256) expected_patch_sha256=$value ;;
    "") ;;
    *) die "unknown manifest key: $key" ;;
    esac
  done <"$manifest"
}

(($# >= 1 && $# <= 2)) || {
  usage
  exit 2
}
manifest=$(readlink -f "$1")
repo=${2:-"$HOME/dotfiles"}
manifest_directory=$(dirname -- "$manifest")
manifest_version=
series_name=
expected_base=
expected_count=
patch_name=
expected_patch_sha256=
read_manifest "$manifest"

[[ $manifest_version == 1 ]] || die "unsupported manifest version"
[[ $series_name =~ ^[a-z0-9][a-z0-9._-]*$ ]] || die "invalid series name"
[[ $expected_base =~ ^[0-9a-f]{40}$ ]] || die "invalid base commit"
[[ $expected_count =~ ^[1-9][0-9]*$ ]] || die "invalid commit count"
[[ $patch_name == "dotfiles-$series_name.patch" ]] || die "invalid patch name"
[[ $expected_patch_sha256 =~ ^[0-9a-f]{64}$ ]] ||
  die "invalid patch checksum"

patch_path="$manifest_directory/$patch_name"
[[ -f $patch_path ]] || die "patch file not found: $patch_path"
actual_patch_sha256=$(sha256sum "$patch_path" | cut -d ' ' -f 1)
[[ $actual_patch_sha256 == "$expected_patch_sha256" ]] ||
  die "patch checksum mismatch"

git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1 ||
  die "not a Git worktree: $repo"
[[ $(git -C "$repo" branch --show-current) == main ]] ||
  die "expected the main branch"
[[ -z $(git -C "$repo" status --porcelain=v1 --untracked-files=all) ]] ||
  die "main worktree is not clean"

git -C "$repo" fetch origin main
head=$(git -C "$repo" rev-parse HEAD)
origin_main=$(git -C "$repo" rev-parse origin/main)
[[ $head == "$origin_main" ]] ||
  die "local main must exactly match origin/main before applying"
[[ $origin_main == "$expected_base" ]] ||
  die "origin/main does not match the patch base"

user_name=$(git -C "$repo" config --get user.name || true)
user_email=$(git -C "$repo" config --get user.email || true)
signing_key=$(git -C "$repo" config --get user.signingkey || true)
[[ -n $user_name ]] || die "Git user.name is not configured"
[[ -n $user_email ]] || die "Git user.email is not configured"
[[ -n $signing_key ]] || die "Git user.signingkey is not configured"
[[ $user_email != "portable@localhost" ]] ||
  die "configure the real destination identity before applying"

mail_directory=$(mktemp -d)
temporary_branch="apply-$series_name-$(date +%Y%m%d%H%M%S)"
success=false

cleanup() {
  local active_branch
  local am_directory

  rm -rf -- "$mail_directory"
  if [[ $success != true ]]; then
    active_branch=$(git -C "$repo" branch --show-current || true)
    am_directory=$(
      git -C "$repo" rev-parse --path-format=absolute \
        --git-path rebase-apply || true
    )
    printf '%s\n' \
      "Application stopped before main was updated." \
      "Temporary branch: $temporary_branch" \
      "Active branch: ${active_branch:-unknown}" >&2
    if [[ -n $am_directory && -d $am_directory ]]; then
      printf 'A git am operation is active; abort it with: git -C %q am --abort\n' \
        "$repo" >&2
    fi
  fi
}
trap cleanup EXIT

git mailsplit --mboxrd -o"$mail_directory" "$patch_path" >/dev/null
shopt -s nullglob
mail_files=("$mail_directory"/*)
[[ ${#mail_files[@]} -eq $expected_count ]] ||
  die "unexpected patch count: ${#mail_files[@]}"

git -C "$repo" switch -c "$temporary_branch"
for mail_file in "${mail_files[@]}"; do
  git -C "$repo" am --3way "$mail_file"
  git -C "$repo" commit --amend --no-edit --reset-author --gpg-sign
done

applied_count=$(git -C "$repo" rev-list --count "$expected_base..HEAD")
[[ $applied_count -eq $expected_count ]] ||
  die "unexpected applied commit count: $applied_count"
(
  cd -- "$repo"
  nix fmt .
  git diff --exit-code
  git diff --cached --exit-code
  nix flake check --show-trace --no-update-lock-file
  nix --extra-experimental-features 'nix-command flakes' run \
    .#home-manager -- build --flake ".#$(whoami)" --show-trace \
    --no-out-link --no-update-lock-file
)

git -C "$repo" switch main
git -C "$repo" merge --ff-only "$temporary_branch"
git -C "$repo" branch -d "$temporary_branch"
success=true

printf '%s\n' \
  "Portable series applied, re-authored, signed, and validated on local main." \
  "Nothing was pushed. Review the commits and push them yourself."
