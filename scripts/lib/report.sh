# shellcheck shell=bash
# Sourced by the repository-resident scripts; it has no shebang of its own.

# Report a fatal condition and return non-zero, leaving the decision to stop
# with the caller. Scripts whose callers cannot act on a return value define
# their own die that exits instead.
die() {
  printf 'error: %s\n' "$*" >&2
  return 1
}
