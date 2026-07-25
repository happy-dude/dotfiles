#!/usr/bin/env bash
# shellcheck disable=SC2329 # cleanup is invoked by the EXIT trap.

# The apply script is the only thing in this repository that rewrites main on
# the destination computer, and it runs there from a transferred copy with no
# tests beside it. Everything asserted here happens before the first mutation:
# each case must refuse and leave main exactly where it was.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
# Invoked through bash throughout: the check sandbox has no /usr/bin/env,
# so the script's shebang cannot be relied on there.
APPLY_SCRIPT="$REPO_DIR/scripts/apply-portable-series.sh"
TMPDIR_TEST="$(mktemp -d)"

export GIT_CONFIG_GLOBAL=/dev/null

cleanup() {
  rm -rf -- "$TMPDIR_TEST"
}
trap cleanup EXIT

fail() {
  printf 'FAIL: %s\n' "$*" >&2
  exit 1
}

git_as() {
  local repo=$1
  shift
  git -C "$repo" \
    -c user.name='Apply Test' \
    -c user.email='apply-test@example.invalid' \
    -c commit.gpgsign=false \
    "$@"
}

# A destination repository whose main matches its origin, as the script demands.
make_destination() {
  local name=$1
  local upstream="$TMPDIR_TEST/$name-origin"
  local repo="$TMPDIR_TEST/$name"

  git init -q --bare --initial-branch=main "$upstream"
  git init -q --initial-branch=main "$repo"
  printf 'base\n' >"$repo/file.txt"
  git -C "$repo" add file.txt
  git_as "$repo" commit -q -m 'seed: create the destination'
  git -C "$repo" remote add origin "$upstream"
  git -C "$repo" push -q origin main
  git -C "$repo" config user.name 'Destination User'
  git -C "$repo" config user.email 'destination@example.invalid'
  git -C "$repo" config user.signingkey 'DEADBEEF'
  printf '%s' "$repo"
}

# A patch of one commit that applies cleanly onto the destination's main.
make_patch() {
  local repo=$1
  local directory=$2

  mkdir -p -- "$directory"
  git -C "$repo" switch -q -c series
  printf 'changed\n' >"$repo/file.txt"
  git -C "$repo" add file.txt
  git_as "$repo" commit -q -m 'series: change the file'
  git -C "$repo" format-patch -q -1 --stdout main \
    >"$directory/dotfiles-probe.patch"
  git -C "$repo" switch -q main
  git -C "$repo" branch -q -D series
}

write_manifest() {
  local directory=$1
  local base=$2
  local checksum=$3
  shift 3

  cat >"$directory/dotfiles-probe.manifest" <<EOF
version=${1:-1}
name=probe
base=$base
count=1
patch=dotfiles-probe.patch
sha256=$checksum
EOF
}

# Run the script and require it to refuse without touching main.
assert_refuses() {
  local description=$1
  local repo=$2
  local manifest=$3
  local before after output status

  before=$(git -C "$repo" rev-parse main)
  set +e
  output=$(bash "$APPLY_SCRIPT" "$manifest" "$repo" 2>&1)
  status=$?
  set -e
  after=$(git -C "$repo" rev-parse main)

  ((status != 0)) || fail "$description: expected a non-zero exit"
  [[ $before == "$after" ]] ||
    fail "$description: main moved from $before to $after"
  [[ $(git -C "$repo" branch --show-current) == main ]] ||
    fail "$description: left the repository on another branch"
  printf '%s' "$output"
}

repo=$(make_destination good)
base=$(git -C "$repo" rev-parse main)
artifacts="$TMPDIR_TEST/artifacts"
make_patch "$repo" "$artifacts"
checksum=$(sha256sum "$artifacts/dotfiles-probe.patch" | cut -d ' ' -f 1)
manifest="$artifacts/dotfiles-probe.manifest"

# A tampered patch must not be applied even though the manifest is well formed.
write_manifest "$artifacts" "$base" "$checksum"
printf '\n' >>"$artifacts/dotfiles-probe.patch"
output=$(assert_refuses 'tampered patch' "$repo" "$manifest")
[[ $output == *"checksum mismatch"* ]] ||
  fail "tampered patch: unexpected message: $output"
git -C "$repo" checkout -q -- . 2>/dev/null || true
make_patch "$repo" "$artifacts"
checksum=$(sha256sum "$artifacts/dotfiles-probe.patch" | cut -d ' ' -f 1)

write_manifest "$artifacts" "$base" "$checksum" 2
output=$(assert_refuses 'manifest version' "$repo" "$manifest")
[[ $output == *"unsupported manifest version"* ]] ||
  fail "manifest version: unexpected message: $output"

write_manifest "$artifacts" 'not-a-commit' "$checksum"
output=$(assert_refuses 'malformed base' "$repo" "$manifest")
[[ $output == *"invalid base commit"* ]] ||
  fail "malformed base: unexpected message: $output"

write_manifest "$artifacts" "$base" 'not-a-checksum'
output=$(assert_refuses 'malformed checksum' "$repo" "$manifest")
[[ $output == *"invalid patch checksum"* ]] ||
  fail "malformed checksum: unexpected message: $output"

# A base naming a different commit than origin/main must be refused, because
# applying it would silently produce a history nobody reviewed.
write_manifest "$artifacts" "$(printf '%040d' 0)" "$checksum"
output=$(assert_refuses 'foreign base' "$repo" "$manifest")
[[ $output == *"does not match the patch base"* ]] ||
  fail "foreign base: unexpected message: $output"

write_manifest "$artifacts" "$base" "$checksum"
printf 'unknown=value\n' >>"$manifest"
output=$(assert_refuses 'unknown manifest key' "$repo" "$manifest")
[[ $output == *"unknown manifest key"* ]] ||
  fail "unknown key: unexpected message: $output"
write_manifest "$artifacts" "$base" "$checksum"

# A dirty destination must be refused before any commit is applied.
printf 'local edit\n' >>"$repo/file.txt"
output=$(assert_refuses 'dirty worktree' "$repo" "$manifest")
[[ $output == *"not clean"* ]] ||
  fail "dirty worktree: unexpected message: $output"
git -C "$repo" checkout -q -- file.txt

# The placeholder identity used while preparing a series must never author
# commits on the destination.
git -C "$repo" config user.email 'portable@localhost'
output=$(assert_refuses 'placeholder identity' "$repo" "$manifest")
[[ $output == *"real destination identity"* ]] ||
  fail "placeholder identity: unexpected message: $output"
git -C "$repo" config user.email 'destination@example.invalid'

git -C "$repo" config --unset user.signingkey
output=$(assert_refuses 'missing signing key' "$repo" "$manifest")
[[ $output == *"signingkey is not configured"* ]] ||
  fail "missing signing key: unexpected message: $output"
git -C "$repo" config user.signingkey 'DEADBEEF'

# main having moved ahead of origin/main means the destination has work the
# series was never rebased onto.
printf 'ahead\n' >"$repo/file.txt"
git -C "$repo" add file.txt
git_as "$repo" commit -q -m 'local: diverge from origin'
output=$(assert_refuses 'main ahead of origin' "$repo" "$manifest")
[[ $output == *"must exactly match origin/main"* ]] ||
  fail "main ahead: unexpected message: $output"
git -C "$repo" reset -q --hard "$base"

# Being on any other branch must be refused, so the series cannot land
# somewhere unnoticed.
git -C "$repo" switch -q -c elsewhere
before=$(git -C "$repo" rev-parse main)
set +e
output=$(bash "$APPLY_SCRIPT" "$manifest" "$repo" 2>&1)
status=$?
set -e
((status != 0)) || fail 'wrong branch: expected a non-zero exit'
[[ $output == *"expected the main branch"* ]] ||
  fail "wrong branch: unexpected message: $output"
[[ $(git -C "$repo" rev-parse main) == "$before" ]] ||
  fail 'wrong branch: main moved'
git -C "$repo" switch -q main

# A missing patch beside a valid manifest must be reported, not ignored.
rm -f -- "$artifacts/dotfiles-probe.patch"
output=$(assert_refuses 'missing patch' "$repo" "$manifest")
[[ $output == *"patch file not found"* ]] ||
  fail "missing patch: unexpected message: $output"

set +e
bash "$APPLY_SCRIPT" >/dev/null 2>&1
status=$?
set -e
((status == 2)) || fail "no arguments: expected exit 2, got $status"

printf 'apply-portable-series guard rails: all cases refused as expected\n'
