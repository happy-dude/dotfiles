#!/usr/bin/env bash
set -euo pipefail

# Usage: ./update-nix-hashes.sh [directory]
# Finds all .nix files with fetchFromGitHub/fetchGit and updates rev/hash pairs

SEARCH_DIR="${1:-.}"

echo "Searching for .nix files in: $SEARCH_DIR"
echo

# Find all nix files with fetchFromGitHub or fetchGit
mapfile -t NIX_FILES < <(find "$SEARCH_DIR" -name "*.nix" -exec grep -lE "(fetchFromGitHub|fetchGit)" {} \; 2>/dev/null)

if [[ ${#NIX_FILES[@]} -eq 0 ]]; then
    echo "No .nix files with fetchFromGitHub or fetchGit found."
    exit 0
fi

echo "Found ${#NIX_FILES[@]} file(s):"
printf '  %s\n' "${NIX_FILES[@]}"
echo

for NIX_FILE in "${NIX_FILES[@]}"; do
    echo "=== Processing: $NIX_FILE ==="

    # Handle fetchFromGitHub blocks
    awk '
        /owner = "/ { gsub(/.*owner = "|";.*/, ""); owner = $0 }
        /repo = "/ { gsub(/.*repo = "|";.*/, ""); print "github", owner, $0 }
    ' "$NIX_FILE" | sort -u | while read -r type owner repo; do
        [[ -z "$owner" || -z "$repo" ]] && continue

        echo "  [fetchFromGitHub] Updating $owner/$repo..."

        if ! result=$(nix-prefetch-github "$owner" "$repo" 2>/dev/null); then
            echo "    ERROR: Failed to fetch $owner/$repo, skipping"
            continue
        fi

        new_rev=$(echo "$result" | jq -r '.rev')
        new_hash=$(echo "$result" | jq -r '.hash')

        if [[ "$new_rev" == "null" || "$new_hash" == "null" ]]; then
            echo "    ERROR: Invalid response for $owner/$repo, skipping"
            continue
        fi

        sed -i -E "
            /owner = \"$owner\";/,/\};/ {
                /repo = \"$repo\";/,/\};/ {
                    s|(rev = \")[^\"]+(\";)|\1$new_rev\2|
                    s|(hash = \")[^\"]+(\";)|\1$new_hash\2|
                }
            }
        " "$NIX_FILE"

        echo "    rev:  $new_rev"
        echo "    hash: $new_hash"
    done

    # Handle fetchGit blocks (extract URL, get latest commit)
    grep -oP 'url = "\Khttps://github\.com/[^"]+' "$NIX_FILE" 2>/dev/null | sort -u | while read -r url; do
        # Clean URL (remove trailing slash or .git)
        clean_url="${url%/}"
        clean_url="${clean_url%.git}"

        # Extract owner/repo from URL
        if [[ "$clean_url" =~ github\.com/([^/]+)/([^/]+) ]]; then
            owner="${BASH_REMATCH[1]}"
            repo="${BASH_REMATCH[2]}"
        else
            echo "    ERROR: Could not parse URL: $url"
            continue
        fi

        echo "  [fetchGit] Updating $owner/$repo..."

        # Get latest commit from default branch using git ls-remote
        if ! new_rev=$(git ls-remote "https://github.com/$owner/$repo" HEAD 2>/dev/null | cut -f1); then
            echo "    ERROR: Failed to get latest commit for $owner/$repo, skipping"
            continue
        fi

        if [[ -z "$new_rev" ]]; then
            echo "    ERROR: Empty rev for $owner/$repo, skipping"
            continue
        fi

        # Escape URL for sed (slashes)
        escaped_url=$(printf '%s\n' "$url" | sed 's/[[\.*^$()+?{|]/\\&/g; s|/|\\/|g')

        # Update rev in the fetchGit block containing this URL
        sed -i -E "
            /url = \"[^\"]*${owner}\/${repo}/,/\};/ {
                s|(rev = \")[^\"]+(\";)|\1$new_rev\2|
            }
        " "$NIX_FILE"

        echo "    rev: $new_rev"
    done

    echo
done

echo "Done! Review changes with: git diff"
