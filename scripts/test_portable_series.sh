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
