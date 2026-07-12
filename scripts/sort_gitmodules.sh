#!/usr/bin/env bash

set -euo pipefail

# ref: https://gist.github.com/jaytaylor/fad7bc69e5f12fc2331e2c6330bd8419#gistcomment-3413386

usage() {
  cat <<'EOF'
Usage: ./scripts/sort_gitmodules.sh [--check]

Sort .gitmodules alphabetically by submodule name.

Options:
  --check     Report whether sorting is needed without changing the file.
  -h, --help  Show this help.
EOF
}

CHECK_ONLY=false

while [ "$#" -gt 0 ]; do
  case "$1" in
  --check)
    CHECK_ONLY=true
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  *)
    printf 'Unknown option: %s\n' "$1" >&2
    usage >&2
    exit 2
    ;;
  esac
  shift
done

GITMODULES=.gitmodules
TMP_FILE=

cleanup() {
  if [ -n "$TMP_FILE" ]; then
    rm -f -- "$TMP_FILE"
  fi
}

trap cleanup EXIT

trap 'exit 129' HUP
trap 'exit 130' INT
trap 'exit 143' TERM

if [ ! -f "$GITMODULES" ]; then
  printf '%s is absent; there are no submodules to sort.\n' "$GITMODULES"
  exit 0
fi

TMP_FILE="$(mktemp "${GITMODULES}.tmp.XXXXXX")"

awk 'BEGIN { J=0 ; K="" } ; /^\[submodule/{ N+=1 ; J=1 ; K=$2 ; gsub(/("vendor\/|["\]])/, "", K) } ; { print K, N, J, $0 } ; { J+=1 }' "$GITMODULES" |
  LC_ALL=C sort -d -f |
  awk '{ $1="" ; $2="" ; $3="" ; print }' |
  sed 's/^ *//g' |
  awk '/^\[/{ print ; next } { print "\t" $0 }' >"$TMP_FILE"

chmod --reference="$GITMODULES" "$TMP_FILE"

if cmp -s -- "$GITMODULES" "$TMP_FILE"; then
  printf '%s is already sorted.\n' "$GITMODULES"
  exit 0
fi

if [ "$CHECK_ONLY" = true ]; then
  printf '%s is not sorted; run %s without --check.\n' "$GITMODULES" "$0" >&2
  exit 1
fi

mv -- "$TMP_FILE" "$GITMODULES"
TMP_FILE=
printf 'Sorted %s.\n' "$GITMODULES"
