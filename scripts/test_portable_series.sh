#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

# Neutralise the user's global Git configuration: the managed commit-msg
# hook lints every commit, so a suite that creates them must not depend on
# whether that hook is installed on the machine running it.
export GIT_CONFIG_GLOBAL=/dev/null

source_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
temporary_directory=$(mktemp -d)
trap 'rm -rf -- "$temporary_directory"' EXIT
repo="$temporary_directory/repo"
mkdir -p "$repo/scripts"
cp -- "$source_root/scripts/portable-series.sh" "$repo/scripts/"
cp -R -- "$source_root/scripts/lib" "$repo/scripts/"
# The source tree may be a read-only Nix store path; the fixture is removed
# on exit, so its copy has to stay writable.
chmod -R u+w "$repo/scripts/lib"

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
scan_forbidden_content secret "changed content" "$patch_path"
test -e "$patch_path"

if validate_forbidden_pattern '[' 2>/dev/null; then
  printf 'accepted an invalid forbidden-content pattern\n' >&2
  exit 1
fi

printf '%s\n' secret >"$patch_path"
scan_error="$temporary_directory/scan-error"
if scan_forbidden_content secret "changed content" "$patch_path" \
  2>"$scan_error"; then
  printf 'accepted forbidden portable content\n' >&2
  exit 1
fi
grep -Eq 'forbidden content found in changed content at line\(s\): 1' \
  "$scan_error"
if grep -q secret "$scan_error"; then
  printf 'exposed forbidden content in scan diagnostics\n' >&2
  exit 1
fi
test -e "$patch_path"

metadata_path="$temporary_directory/metadata"
content_path="$temporary_directory/content"
cat >"$metadata_path.source" <<'EOF'
tests: keep required attribution

Assisted-by: ChatGPT (private-model, medium, Codex)
EOF
git interpret-trailers \
  --if-exists replace \
  --if-missing doNothing \
  --trailer 'Assisted-by:' \
  <"$metadata_path.source" >"$metadata_path"
scan_forbidden_content private-model "commit metadata" \
  "$metadata_path"

cat >"$metadata_path.source" <<'EOF'
tests: retain ordinary message content

Assisted-by: private-model appears in the message body.

This paragraph prevents the preceding line from being a trailer.
EOF
git interpret-trailers \
  --if-exists replace \
  --if-missing doNothing \
  --trailer 'Assisted-by:' \
  <"$metadata_path.source" >"$metadata_path"
if scan_forbidden_content private-model "commit metadata" "$metadata_path" \
  2>/dev/null; then
  printf 'ignored a non-trailer attribution line in commit metadata\n' >&2
  exit 1
fi

printf '%s\n' '+Assisted-by: ChatGPT (private-model, medium, Codex)' \
  >"$content_path"
if scan_forbidden_content private-model "changed content" "$content_path" \
  2>/dev/null; then
  printf 'ignored an attribution-shaped line in changed content\n' >&2
  exit 1
fi

cat >"$content_path" <<'EOF'
Subject: [PATCH] tests: preserve attribution

Assisted-by: ChatGPT (private-model, medium, Codex)

diff --git i/example w/example
--- i/example
+++ w/example
@@ -0,0 +1 @@
+Assisted-by: ChatGPT (private-model, medium, Codex)
EOF
if scan_final_patch private-model "$content_path" "$temporary_directory" \
  2>/dev/null; then
  printf 'ignored an attribution-shaped line in final changed content\n' >&2
  exit 1
fi

staging_directory="$temporary_directory/staging"
output_directory="$temporary_directory/output"
mkdir "$staging_directory" "$output_directory"
printf '%s\n' patch >"$staging_directory/series.patch"
printf '%s\n' manifest >"$staging_directory/series.manifest"
printf '%s\n' checksums >"$staging_directory/series.sha256"
printf '%s\n' apply >"$staging_directory/apply.sh"
create_bundle \
  "$staging_directory" bundle series.tar.gz series.tar.gz.sha256 \
  series.patch series.manifest series.sha256 apply.sh
(
  cd -- "$staging_directory"
  sha256sum -c series.tar.gz.sha256 >/dev/null
  tar -tzf series.tar.gz | grep -Eq '^bundle/apply\.sh$'
)

printf '%s\n' old >"$output_directory/series.patch"
printf '%s\n' old >"$output_directory/series.sha256"
rm "$staging_directory/series.manifest"
if publish_artifacts \
  "$staging_directory" "$output_directory" \
  series.sha256 series.tar.gz.sha256 \
  series.patch series.manifest apply.sh series.tar.gz 2>/dev/null; then
  printf 'published an incomplete artifact set\n' >&2
  exit 1
fi
test "$(<"$output_directory/series.patch")" = old
test "$(<"$output_directory/series.sha256")" = old

attached_worktree="$temporary_directory/attached-worktree"
git branch replay/attached
git worktree add --quiet "$attached_worktree" replay/attached
start_error="$temporary_directory/start-error"
if start_series attached "$temporary_directory/new-attached" \
  2>"$start_error"; then
  printf 'reused a replay branch with an attached worktree\n' >&2
  exit 1
fi
grep -Fq \
  "branch already exists with worktree: replay/attached ($attached_worktree)" \
  "$start_error"
grep -Fq "Continue the existing series in: $attached_worktree" \
  "$start_error"
printf -v expected_export '  %q export %q' \
  "$repo/scripts/portable-series.sh" attached
grep -Fq "$expected_export" "$start_error"
git worktree remove --force "$attached_worktree"
git branch -D replay/attached >/dev/null

git branch replay/unattached
if start_series unattached "$temporary_directory/new-unattached" \
  2>"$start_error"; then
  printf 'reused an unattached replay branch\n' >&2
  exit 1
fi
grep -Fq \
  'branch already exists without a worktree: replay/unattached' \
  "$start_error"
printf -v expected_attach '  git -C %q worktree add <path> %q' \
  "$repo" replay/unattached
grep -Fq "$expected_attach" "$start_error"
git branch -D replay/unattached >/dev/null

prunable_worktree="$temporary_directory/prunable-worktree"
git branch replay/prunable
git worktree add --quiet "$prunable_worktree" replay/prunable
rm -rf -- "$prunable_worktree"
if start_series prunable "$temporary_directory/new-prunable" \
  2>"$start_error"; then
  printf 'reused a replay branch with a prunable worktree\n' >&2
  exit 1
fi
grep -Fq \
  'branch already exists with prunable worktree registration: replay/prunable' \
  "$start_error"
printf -v expected_inspect '  git -C %q worktree list --porcelain' "$repo"
grep -Fq "$expected_inspect" "$start_error"
if export_series prunable "$output_directory" 2>"$start_error"; then
  printf 'exported from a prunable worktree\n' >&2
  exit 1
fi
grep -Fq \
  'worktree registration is prunable for branch: replay/prunable' \
  "$start_error"
git worktree prune
git branch -D replay/prunable >/dev/null

existing_path="$temporary_directory/existing-worktree"
mkdir "$existing_path"
if start_series existing-path "$existing_path" 2>"$start_error"; then
  printf 'reused an existing worktree path\n' >&2
  exit 1
fi
grep -Fq "worktree path already exists: $existing_path" "$start_error"

for series_name in one two; do
  series_staging="$temporary_directory/staging-$series_name"
  apply_name=$(apply_artifact_name "$series_name")
  mkdir "$series_staging"
  printf '%s patch\n' "$series_name" >"$series_staging/$series_name.patch"
  printf '%s manifest\n' "$series_name" \
    >"$series_staging/$series_name.manifest"
  printf '%s apply\n' "$series_name" >"$series_staging/$apply_name"
  printf '%s bundle\n' "$series_name" \
    >"$series_staging/$series_name.tar.gz"
  (
    cd -- "$series_staging"
    sha256sum \
      "$series_name.patch" "$series_name.manifest" "$apply_name" \
      >"$series_name.sha256"
    sha256sum "$series_name.tar.gz" >"$series_name.tar.gz.sha256"
  )
  publish_artifacts \
    "$series_staging" "$output_directory" \
    "$series_name.sha256" "$series_name.tar.gz.sha256" \
    "$series_name.patch" "$series_name.manifest" "$apply_name" \
    "$series_name.tar.gz"
done
(
  cd -- "$output_directory"
  sha256sum -c one.sha256 >/dev/null
  sha256sum -c two.sha256 >/dev/null
)
test -f "$output_directory/apply-dotfiles-one.sh"
test -f "$output_directory/apply-dotfiles-two.sh"

# clean retires a series whose commits are represented upstream and refuses
# one carrying unrepresented work or a dirty worktree.
upstream="$temporary_directory/upstream.git"
git init --quiet --bare --initial-branch=main "$upstream"
git remote add origin "$upstream"
git push --quiet origin main
git fetch --quiet origin refs/heads/main:refs/remotes/origin/main

series_worktree="$temporary_directory/series-demo"
start_series demo "$series_worktree" >/dev/null
printf '%s\n' represented >"$series_worktree/represented"
git -C "$series_worktree" add represented
git -C "$series_worktree" commit --quiet -m 'tests: add represented change'

if clean_series demo 2>/dev/null; then
  printf 'cleaned a series with unrepresented commits\n' >&2
  exit 1
fi
test -d "$series_worktree"

# The same patch lands upstream (re-authored, as the apply script does).
printf '%s\n' represented >"$repo/represented"
git add represented
git commit --quiet -m 'tests: add represented change (upstream copy)'
git push --quiet origin main
git fetch --quiet origin refs/heads/main:refs/remotes/origin/main

printf '%s\n' dirty >"$series_worktree/dirty"
if clean_series demo 2>/dev/null; then
  printf 'cleaned a series with a dirty worktree\n' >&2
  exit 1
fi
rm -- "$series_worktree/dirty"

clean_series demo >/dev/null
test ! -e "$series_worktree"
if git show-ref --verify --quiet refs/heads/replay/demo; then
  printf 'clean left the series branch behind\n' >&2
  exit 1
fi

if clean_series never-started 2>/dev/null; then
  printf 'cleaned a series that never existed\n' >&2
  exit 1
fi

# An invalid series name is refused before any state is created.
if clean_series 'bad name' 2>/dev/null; then
  printf 'accepted an invalid series name\n' >&2
  exit 1
fi

# clean covers the remaining worktree_for_branch outcomes: a prunable
# registration is refused, and a branch with no worktree is simply deleted.
git branch replay/prune refs/remotes/origin/main
prune_worktree="$temporary_directory/prune-wt"
git worktree add --quiet "$prune_worktree" replay/prune
rm -rf -- "$prune_worktree"
if clean_series prune 2>/dev/null; then
  printf 'cleaned a series with a prunable worktree\n' >&2
  exit 1
fi
git show-ref --verify --quiet refs/heads/replay/prune || {
  printf '%s\n' 'prunable refusal deleted the branch' >&2
  exit 1
}
git worktree prune
git branch -D replay/prune >/dev/null

git branch replay/detached refs/remotes/origin/main
clean_series detached >/dev/null
if git show-ref --verify --quiet refs/heads/replay/detached; then
  printf 'clean left a worktree-less series branch behind\n' >&2
  exit 1
fi
