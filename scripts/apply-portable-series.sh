#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# This script is transferred to the destination computer on its own, so it
# must not source anything from this repository.
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

  while IFS='=' read -r key value || [[ -n $key ]]; do
    value=${value%$'\r'}
    case $key in
    version) manifest_version=$value ;;
    name) series_name=$value ;;
    base) expected_base=$value ;;
    count) expected_count=$value ;;
    patch) patch_name=$value ;;
    sha256) expected_patch_sha256=$value ;;
    tree) expected_tree=$value ;;
    messages) expected_message_digest=$value ;;
    "") ;;
    *) die "unknown manifest key: $key" ;;
    esac
  done <"$manifest"
}

# Digest of the commit messages in <base>..HEAD: SHA-256 over each message as
# its commit object stores it (the bytes after the header's blank line),
# oldest commit first, each followed by one NUL byte. portable-series.sh
# records the same digest in the manifest with its own copy of this function;
# keep the two identical.
series_message_digest() {
  local repository=$1
  local base=$2
  local commits
  local commit
  local digest

  commits=$(git -C "$repository" rev-list --reverse "$base..HEAD") || return 1
  digest=$(
    while IFS= read -r commit; do
      [[ -n $commit ]] || continue
      git -C "$repository" cat-file commit "$commit" | sed '1,/^$/d' || exit 1
      printf '\0'
    done <<<"$commits" | sha256sum
  ) || return 1
  printf '%s\n' "${digest%% *}"
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
expected_tree=
expected_message_digest=
read_manifest "$manifest"

# A version 1 manifest records neither the tree nor the messages checked
# below. Export writes the manifest and this script together, so the remedy
# is a fresh export.
[[ $manifest_version != 1 ]] ||
  die "manifest version 1 records no tree or message digest; export the series again and run the apply script that export writes"
[[ $manifest_version == 2 ]] ||
  die "unsupported manifest version; this script applies version 2 only"
[[ $series_name =~ ^[a-z0-9][a-z0-9._-]*$ ]] || die "invalid series name"
[[ $expected_base =~ ^[0-9a-f]{40}$ ]] || die "invalid base commit"
[[ $expected_count =~ ^[1-9][0-9]*$ ]] || die "invalid commit count"
[[ $patch_name == "dotfiles-$series_name.patch" ]] || die "invalid patch name"
[[ $expected_patch_sha256 =~ ^[0-9a-f]{64}$ ]] ||
  die "invalid patch checksum"
[[ $expected_tree =~ ^[0-9a-f]{40}$ ]] || die "invalid validated tree"
[[ $expected_message_digest =~ ^[0-9a-f]{64}$ ]] ||
  die "invalid commit message digest"

patch_path="$manifest_directory/$patch_name"
[[ -f $patch_path ]] || die "patch file not found: $patch_path"
actual_patch_sha256=$(sha256sum "$patch_path" | cut -d ' ' -f 1)
[[ $actual_patch_sha256 == "$expected_patch_sha256" ]] ||
  die "patch checksum mismatch"

git -C "$repo" rev-parse --is-inside-work-tree >/dev/null 2>&1 ||
  die "not a Git worktree: $repo"
[[ $(git -C "$repo" branch --show-current) == main ]] ||
  die "expected the main branch"
main_status=$(git -C "$repo" status --porcelain=v1 --untracked-files=all) ||
  die "unable to read the main worktree status: $repo"
[[ -z $main_status ]] || die "main worktree is not clean"

git -C "$repo" fetch origin refs/heads/main:refs/remotes/origin/main
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
[[ $user_name != "Portable Dotfiles" ]] ||
  die "configure the real destination identity before applying"

mail_directory=$(mktemp -d)
temporary_branch="apply-$series_name-$(date +%Y%m%d%H%M%S)"
success=false

cleanup() {
  local active_branch
  local am_directory

  rm -rf -- "$mail_directory"
  [[ $success != true ]] || return 0
  # A failure before switch -c leaves main checked out and nothing to clean.
  git -C "$repo" show-ref --verify --quiet "refs/heads/$temporary_branch" ||
    return 0
  active_branch=$(git -C "$repo" branch --show-current || true)
  am_directory=$(
    git -C "$repo" rev-parse --path-format=absolute \
      --git-path rebase-apply || true
  )
  printf '%s\n' \
    "Application stopped before main was updated." \
    "Temporary branch: $temporary_branch" \
    "Active branch: ${active_branch:-unknown}" \
    "Discard the partial application with:" >&2
  # In this order: switch refuses while git am is in progress, and branch -D
  # refuses the checked-out branch.
  if [[ -n $am_directory && -d $am_directory ]]; then
    printf '  git -C %q am --abort\n' "$repo" >&2
  fi
  printf '  git -C %q switch main\n' "$repo" >&2
  printf '  git -C %q branch -D %q\n' "$repo" "$temporary_branch" >&2
  printf '%s\n' \
    "Or keep the temporary branch and inspect what was applied." >&2
}
trap cleanup EXIT

git mailsplit -o"$mail_directory" "$patch_path" >/dev/null
shopt -s nullglob
mail_files=("$mail_directory"/*)
[[ ${#mail_files[@]} -eq $expected_count ]] ||
  die "unexpected patch count: ${#mail_files[@]}"

git -C "$repo" switch -c "$temporary_branch"
for mail_file in "${mail_files[@]}"; do
  # --whitespace=nowarn overrides the destination's apply.whitespace, which
  # would rewrite the content, and --keep-non-patch keeps a leading [token]
  # in the subject. Neither may change what export validated.
  git -C "$repo" am --3way --keep-non-patch --whitespace=nowarn "$mail_file"
  git -C "$repo" commit --amend --no-edit --reset-author --gpg-sign
done

applied_count=$(git -C "$repo" rev-list --count "$expected_base..HEAD")
[[ $applied_count -eq $expected_count ]] ||
  die "unexpected applied commit count: $applied_count"
# git am can still land something other than the validated series: mailinfo
# reads parts of a message as mail structure, and the destination's Git
# configuration is not the exporter's. Compare before the validation below so
# a different series is never built, and before main moves.
applied_tree=$(git -C "$repo" rev-parse 'HEAD^{tree}')
[[ $applied_tree == "$expected_tree" ]] ||
  die "applied tree does not match the tree the export validated"
applied_message_digest=$(series_message_digest "$repo" "$expected_base")
[[ $applied_message_digest == "$expected_message_digest" ]] ||
  die "applied commit messages do not match the messages the export validated"
(
  cd -- "$repo"
  nix fmt --no-update-lock-file .
  # Untracked formatter output is drift too, so compare the full status.
  format_status=$(git status --porcelain=v1 --untracked-files=all) ||
    die "unable to read the main worktree status after formatting"
  if [[ -n $format_status ]]; then
    printf '%s\n' "$format_status" >&2
    die "formatter changed the applied series"
  fi
  nix flake check --show-trace --no-update-lock-file
  nix run --no-update-lock-file .#home-manager -- build --flake ".#$(whoami)" \
    --show-trace --no-out-link --no-update-lock-file
)

git -C "$repo" switch main
git -C "$repo" merge --ff-only "$temporary_branch"
success=true
git -C "$repo" branch -d "$temporary_branch"

printf '%s\n' \
  "Portable series applied, re-authored, signed, and validated on local main." \
  "Nothing was pushed. Review the commits and push them yourself."
