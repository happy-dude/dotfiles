#!/usr/bin/env bash

# The apply script is the only thing in this repository that rewrites main on
# the destination computer, and it runs there from a transferred copy with no
# tests beside it. The first cases stop before the first mutation: each must
# refuse and leave main exactly where it was. The round trip then exports a
# real series and applies it, and the cases after it stop on the temporary
# branch, where main must still not move.

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd -- "$SCRIPT_DIR/.." && pwd)"
# Invoked through bash throughout: the check sandbox has no /usr/bin/env,
# so the script's shebang cannot be relied on there.
APPLY_SCRIPT="$REPO_DIR/scripts/apply-portable-series.sh"

# shellcheck disable=SC1091 # Sourced from the repository.
source "$REPO_DIR/scripts/lib/test-helpers.sh"
test_setup

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
  git -C "$repo" format-patch -q --stdout main..series \
    >"$directory/dotfiles-probe.patch"
  git -C "$repo" switch -q main
  git -C "$repo" branch -q -D series
}

write_manifest() {
  local directory=$1
  local base=$2
  local checksum=$3
  shift 3

  # The default tree and digest are well formed but name nothing: the cases
  # using them refuse before either is compared.
  cat >"$directory/dotfiles-probe.manifest" <<EOF
version=${1:-2}
name=probe
base=$base
count=1
patch=dotfiles-probe.patch
sha256=$checksum
tree=${2:-$(printf '%040d' 1)}
messages=${3:-$(printf '%064d' 1)}
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

write_manifest "$artifacts" "$base" "$checksum" 3
output=$(assert_refuses 'manifest version' "$repo" "$manifest")
[[ $output == *"unsupported manifest version"* ]] ||
  fail "manifest version: unexpected message: $output"

# A version 1 manifest records neither the tree nor the messages, so it is
# refused with the remedy rather than applied unverified.
write_manifest "$artifacts" "$base" "$checksum" 1
output=$(assert_refuses 'manifest version 1' "$repo" "$manifest")
[[ $output == *"manifest version 1 records no tree or message digest"* &&
  $output == *"export the series again"* ]] ||
  fail "manifest version 1: unexpected message: $output"

write_manifest "$artifacts" "$base" "$checksum" 2 'not-a-tree'
output=$(assert_refuses 'malformed tree' "$repo" "$manifest")
[[ $output == *"invalid validated tree"* ]] ||
  fail "malformed tree: unexpected message: $output"

write_manifest "$artifacts" "$base" "$checksum" 2 '' 'not-a-digest'
output=$(assert_refuses 'malformed message digest' "$repo" "$manifest")
[[ $output == *"invalid commit message digest"* ]] ||
  fail "malformed message digest: unexpected message: $output"

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

# A broken submodule makes git status fail while fetch still works, so only
# the status check can stop this.
broken=$(make_destination broken)
git init -q --initial-branch=main "$TMPDIR_TEST/broken-sub"
git_as "$TMPDIR_TEST/broken-sub" commit -q --allow-empty -m 'sub: seed'
git -C "$broken" -c protocol.file.allow=always \
  submodule -q add "$TMPDIR_TEST/broken-sub" sub
git_as "$broken" commit -q -m 'seed: add a submodule'
git -C "$broken" push -q origin main
broken_artifacts="$TMPDIR_TEST/broken-artifacts"
make_patch "$broken" "$broken_artifacts"
write_manifest "$broken_artifacts" "$(git -C "$broken" rev-parse main)" \
  "$(sha256sum "$broken_artifacts/dotfiles-probe.patch" | cut -d ' ' -f 1)"
rm -rf -- "$broken/.git/modules/sub/objects"
output=$(
  assert_refuses 'unreadable status' "$broken" \
    "$broken_artifacts/dotfiles-probe.manifest"
)
[[ $output == *"unable to read the main worktree status"* ]] ||
  fail "unreadable status: unexpected message: $output"

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

# The apply script cannot source the export script, so each carries its own
# series_message_digest. The round trip below shows that the two agree on one
# series; this holds the two definitions to the same text.
function_text() {
  sed -n '/^series_message_digest() {$/,/^}$/p' "$1"
}
export_copy=$(function_text "$REPO_DIR/scripts/portable-series.sh")
apply_copy=$(function_text "$APPLY_SCRIPT")
[[ $export_copy == *'sha256sum'* && $export_copy == *$'\n}' ]] ||
  fail 'series_message_digest was not found in portable-series.sh'
[[ $export_copy == "$apply_copy" ]] ||
  fail 'the two copies of series_message_digest differ'

# Round trip: a real export applied to a destination whose Git configuration
# would rewrite the series. Only the Nix boundary and the signing program are
# substituted; Git, the linter, mailsplit, and git am are real.
fake_bin="$TMPDIR_TEST/bin"
nix_log="$TMPDIR_TEST/nix.log"
mkdir -p "$fake_bin"
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/nix"
cat >>"$fake_bin/nix" <<'EOF'
set -euo pipefail
printf '%s\n' "$1" >>"$PORTABLE_TEST_NIX_LOG"
case $1 in
fmt | flake | build | run) ;;
eval) printf 'first\n' ;;
*) exit 91 ;;
esac
EOF
# git commit --gpg-sign runs gpg.program with --status-fd=2 and requires a
# signature on stdout and a SIG_CREATED status line.
printf '#!%s\n' "$(command -v bash)" >"$fake_bin/sign"
cat >>"$fake_bin/sign" <<'EOF'
cat >/dev/null
printf '%s\n' '-----BEGIN PGP SIGNATURE-----' '' 'stub' \
  '-----END PGP SIGNATURE-----'
printf '%s\n' '[GNUPG:] BEGIN_SIGNING' '[GNUPG:] SIG_CREATED D 1 8 00 0 0' >&2
EOF
chmod 0755 "$fake_bin/nix" "$fake_bin/sign"
export PATH="$fake_bin:$PATH"
export PORTABLE_TEST_NIX_LOG="$nix_log"

trip_origin="$TMPDIR_TEST/trip-origin.git"
trip_seed="$TMPDIR_TEST/trip-seed"
trip_source="$TMPDIR_TEST/trip-source"
trip_series="$TMPDIR_TEST/trip-series"
trip_output="$TMPDIR_TEST/trip-output"
mkdir -p "$trip_seed/scripts" "$trip_output"
cp -- "$REPO_DIR/scripts/portable-series.sh" "$APPLY_SCRIPT" \
  "$REPO_DIR/scripts/lint_commit_message.py" "$trip_seed/scripts/"
cp -R -- "$REPO_DIR/scripts/lib" "$trip_seed/scripts/"
# The source tree may be a read-only Nix store path.
chmod -R u+w "$trip_seed/scripts"
printf 'first line\n' >"$trip_seed/notes"
git init -q --bare --initial-branch=main "$trip_origin"
git -C "$trip_seed" init -q --initial-branch=main
commit_all "$trip_seed" 'tests: create the round-trip base'
git -C "$trip_seed" remote add origin "$trip_origin"
git -C "$trip_seed" push -q -u origin main
trip_base=$(git -C "$trip_seed" rev-parse HEAD)
git clone -q "$trip_origin" "$trip_source"
(
  cd -- "$trip_source"
  bash scripts/portable-series.sh start trip "$trip_series"
) >/dev/null 2>&1

# What git am rewrites unless told otherwise: a trailing space that
# apply.whitespace=fix strips, and a leading bracketed token in a subject.
printf 'set showbreak=x\\ \nplain\n' >"$trip_series/trailing.vim"
git -C "$trip_series" add trailing.vim
git -C "$trip_series" commit -q -m 'vim: add a line ending in a space'
printf 'second line\n' >>"$trip_series/notes"
git -C "$trip_series" commit -q -a -F - <<'EOF'
[x] scripts: keep a bracketed subject

The body spans two paragraphs so that a truncated message is caught, and it
carries text outside ASCII: naïve — 中文.

The trailer below must reach the destination.

Assisted-by: example-model:high harness
EOF
trip_tree=$(git -C "$trip_series" rev-parse 'HEAD^{tree}')
(
  cd -- "$trip_source"
  bash scripts/portable-series.sh export trip "$trip_output"
) >"$TMPDIR_TEST/trip-export.log" 2>&1 ||
  fail "round trip: export failed: $(<"$TMPDIR_TEST/trip-export.log")"
trip_manifest="$trip_output/dotfiles-trip.manifest"
grep -Fxq 'version=2' "$trip_manifest" ||
  fail 'round trip: export did not write a version 2 manifest'
grep -Fxq "tree=$trip_tree" "$trip_manifest" ||
  fail 'round trip: the manifest does not record the validated tree'

# A destination clone of the same origin carrying the settings from
# git/default.nix that change what git am lands, or that a fetch has to
# survive.
make_trip_destination() {
  local repo="$TMPDIR_TEST/$1"

  git clone -q "$trip_origin" "$repo"
  git -C "$repo" config user.name 'Destination User'
  git -C "$repo" config user.email 'destination@example.invalid'
  git -C "$repo" config user.signingkey 'DEADBEEF'
  git -C "$repo" config gpg.program "$fake_bin/sign"
  git -C "$repo" config apply.whitespace fix
  git -C "$repo" config fetch.prune true
  git -C "$repo" config fetch.pruneTags true
  printf '%s' "$repo"
}

# Run the transferred copy of the apply script, as the destination does.
run_trip_apply() {
  local manifest=$1
  local repo=$2

  : >"$nix_log"
  set +e
  trip_log=$(bash "$trip_output/apply-dotfiles-trip.sh" "$manifest" "$repo" 2>&1)
  trip_status=$?
  set -e
}

destination=$(make_trip_destination trip-destination)
run_trip_apply "$trip_manifest" "$destination"
((trip_status == 0)) || fail "round trip: apply failed: $trip_log"
[[ $trip_log == *"Nothing was pushed."* ]] ||
  fail "round trip: apply did not say that nothing was pushed: $trip_log"
[[ $(git -C "$destination" branch --show-current) == main ]] ||
  fail 'round trip: apply left the destination on another branch'
[[ $(git -C "$destination" rev-list --count "$trip_base..main") == 2 &&
$(git -C "$destination" rev-parse 'main~2') == "$trip_base" ]] ||
  fail 'round trip: main did not advance by exactly the two exported commits'
[[ $(git -C "$destination" rev-parse 'main^{tree}') == "$trip_tree" ]] ||
  fail 'round trip: the applied tree differs from the exported tree'
[[ $(sed -n 1p "$destination/trailing.vim") == 'set showbreak=x\ ' ]] ||
  fail 'round trip: the significant trailing space was stripped'
for revision in 'HEAD~1' HEAD; do
  applied_message=$(git -C "$destination" log -1 --format=%B "$revision")
  source_message=$(git -C "$trip_series" log -1 --format=%B "$revision")
  [[ $applied_message == "$source_message" ]] ||
    fail "round trip: the message of $revision differs from its source"
  git -C "$destination" cat-file commit "$revision" | grep -q '^gpgsig ' ||
    fail "round trip: $revision is not signed"
done
[[ $(git -C "$destination" log -1 --format=%s HEAD) == '[x] '* ]] ||
  fail 'round trip: the bracketed subject token was stripped'
identities=$(
  git -C "$destination" log --format='%an <%ae> / %cn <%ce>' \
    "$trip_base..main" | sort -u
)
[[ $identities == 'Destination User <destination@example.invalid> / Destination User <destination@example.invalid>' ]] ||
  fail "round trip: commits lack the destination identity: $identities"
[[ -z $(git -C "$destination" for-each-ref 'refs/heads/apply-*') ]] ||
  fail 'round trip: apply left its temporary branch behind'
[[ $(git -C "$destination" rev-parse refs/remotes/origin/main) == "$trip_base" &&
$(git -C "$trip_origin" rev-parse main) == "$trip_base" ]] ||
  fail 'round trip: origin/main was removed or the series was pushed'
[[ $(paste -sd, - <"$nix_log") == fmt,flake,run ]] ||
  fail "round trip: unexpected validation steps: $(paste -sd, - <"$nix_log")"

# The remaining cases stop on the temporary branch: before validation, before
# main moves, and with the commands that discard what was applied.
assert_stops_before_main() {
  local description=$1
  local repo=$2
  local manifest=$3
  local expected=$4
  local temporary_branch
  local remaining
  local line
  local -a recovery=()

  run_trip_apply "$manifest" "$repo"
  ((trip_status != 0)) || fail "$description: expected a non-zero exit"
  [[ $trip_log == *"$expected"* ]] ||
    fail "$description: unexpected message: $trip_log"
  [[ $(git -C "$repo" rev-parse main) == "$trip_base" ]] ||
    fail "$description: main moved"
  [[ ! -s $nix_log ]] ||
    fail "$description: validation ran on a series export did not validate"
  temporary_branch=$(sed -n 's/^Temporary branch: //p' <<<"$trip_log")
  [[ $(git -C "$repo" branch --show-current) == "$temporary_branch" ]] ||
    fail "$description: not stopped on the temporary branch"

  # git switch refuses while git am is in progress and branch -D refuses the
  # checked-out branch, so the order is part of the contract.
  if [[ -d $repo/.git/rebase-apply ]]; then
    printf -v line '  git -C %q am --abort' "$repo"
    recovery+=("$line")
  elif [[ $trip_log == *"am --abort"* ]]; then
    fail "$description: offered am --abort with no git am in progress"
  fi
  printf -v line '  git -C %q switch main' "$repo"
  recovery+=("$line")
  printf -v line '  git -C %q branch -D %q' "$repo" "$temporary_branch"
  recovery+=("$line" 'Or keep the temporary branch')
  remaining=$trip_log
  for line in "${recovery[@]}"; do
    [[ $remaining == *"$line"* ]] ||
      fail "$description: recovery output lacks, or misorders: $line"
    remaining=${remaining#*"$line"}
  done

  # Run the commands exactly as printed.
  while IFS= read -r line; do
    [[ $line == '  git -C '* ]] || continue
    eval "$line" >/dev/null 2>&1 ||
      fail "$description: printed recovery command failed: $line"
  done <<<"$trip_log"
  [[ $(git -C "$repo" branch --show-current) == main &&
  $(git -C "$repo" rev-parse main) == "$trip_base" &&
  ! -d $repo/.git/rebase-apply &&
  -z $(git -C "$repo" status --porcelain=v1 --untracked-files=all) &&
  -z $(git -C "$repo" for-each-ref 'refs/heads/apply-*') ]] ||
    fail "$description: the printed commands did not restore a clean main"
}

# A manifest naming another tree, or other messages, than the series lands.
destination=$(make_trip_destination trip-mismatch)
altered="$TMPDIR_TEST/trip-altered"
mkdir -p "$altered"
cp -- "$trip_output/dotfiles-trip.patch" "$altered/"
sed "s/^tree=.*/tree=$(git -C "$destination" rev-parse "$trip_base^{tree}")/" \
  "$trip_manifest" >"$altered/dotfiles-trip.manifest"
assert_stops_before_main 'wrong tree' "$destination" \
  "$altered/dotfiles-trip.manifest" 'applied tree does not match'
sed "s/^messages=.*/messages=$(printf '%064d' 1)/" \
  "$trip_manifest" >"$altered/dotfiles-trip.manifest"
assert_stops_before_main 'wrong message digest' "$destination" \
  "$altered/dotfiles-trip.manifest" 'applied commit messages do not match'

# A patch that stops git am partway leaves it in progress on the temporary
# branch, after the first commit was applied.
sed 's/^ first line$/ another line/' "$trip_output/dotfiles-trip.patch" \
  >"$altered/dotfiles-trip.patch"
sed "s/^sha256=.*/sha256=$(
  sha256sum "$altered/dotfiles-trip.patch" | cut -d ' ' -f 1
)/" "$trip_manifest" >"$altered/dotfiles-trip.manifest"
assert_stops_before_main 'conflicting patch' "$destination" \
  "$altered/dotfiles-trip.manifest" 'Application stopped before main'
[[ $trip_log == *"am --abort"* ]] ||
  fail 'conflicting patch: git am was not left in progress'

# The destination has fetch.prune set, so the refresh of origin/main must
# neither delete it nor leave it stale once upstream has moved.
printf 'upstream\n' >"$trip_seed/upstream"
commit_all "$trip_seed" 'tests: advance upstream past the series base'
git -C "$trip_seed" push -q origin main
run_trip_apply "$trip_manifest" "$destination"
((trip_status != 0)) || fail 'moved upstream: expected a non-zero exit'
[[ $trip_log == *"must exactly match origin/main"* ]] ||
  fail "moved upstream: unexpected message: $trip_log"
upstream=$(git -C "$trip_seed" rev-parse HEAD)
[[ $(git -C "$destination" rev-parse refs/remotes/origin/main) == "$upstream" ]] ||
  fail 'moved upstream: origin/main was removed or left stale'
[[ $(git -C "$destination" rev-parse main) == "$trip_base" ]] ||
  fail 'moved upstream: main moved'

printf 'apply-portable-series: all cases passed\n'
