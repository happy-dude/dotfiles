#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

source_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT
repo="$temporary_directory/repo"
mkdir -p "$repo/scripts"
cp -- "$source_root/scripts/portable-series.sh" "$repo/scripts/"

cd -- "$repo"
git init --quiet --initial-branch=main
git config user.name "Portable Dotfiles"
git config user.email "portable@localhost"
git config commit.gpgsign false
git config core.hooksPath /dev/null
printf '%s\n' base >fixture
git add fixture
git commit --quiet -m 'tests: create portable-series base'
base=$(git rev-parse HEAD)

# shellcheck source=/dev/null
source scripts/portable-series.sh
reject_merge_commits "$repo" "$base"

git switch --quiet -c topic
printf '%s\n' topic >topic
git add topic
git commit --quiet -m 'tests: add topic change'
git switch --quiet main
printf '%s\n' main >main
git add main
git commit --quiet -m 'tests: add main change'
git merge --quiet --no-ff topic -m 'tests: merge topic change'
if reject_merge_commits "$repo" "$base" 2>/dev/null; then
  printf 'accepted portable history containing a merge commit\n' >&2
  exit 1
fi

patch_path="$temporary_directory/portable.patch"
printf '%s\n' safe >"$patch_path"
validate_forbidden_pattern secret
scan_forbidden_content secret "$patch_path"
test -e "$patch_path"

if validate_forbidden_pattern '[' 2>/dev/null; then
  printf 'accepted an invalid forbidden-content pattern\n' >&2
  exit 1
fi

printf '%s\n' secret >"$patch_path"
if scan_forbidden_content secret "$patch_path" 2>/dev/null; then
  printf 'accepted forbidden portable content\n' >&2
  exit 1
fi
test ! -e "$patch_path"
