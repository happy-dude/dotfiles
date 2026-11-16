# shellcheck shell=bash
# Run by the git stub that test_setup puts first on PATH; it has no shebang
# of its own.
#
# Usage: test-git-guard.sh <root> <real-git> [git arguments...]
#
# Refuse a Git command that would act outside <root>, the suite's temporary
# directory, and otherwise run it with <real-git>. The directory Git would
# work in is the working directory after every -C, and any repository named
# by --git-dir, --work-tree, or a Git location variable must be inside <root>
# as well. A refusal exits with status 97 before Git runs.
#
# This reads only Git's global options. A path given to a subcommand, such as
# the directory of git init or git clone, is not checked.

set -uo pipefail

root=$1
real_git=$2
shift 2

refuse() {
  printf 'test guard: refusing a Git command outside %s: %s\n' "$root" "$1" >&2
  exit 97
}

# Print the physical path of the nearest existing ancestor of a path, which
# may name something Git is about to create.
nearest_directory() {
  local candidate=$1

  [[ $candidate == /* ]] || candidate="$directory/$candidate"
  while [[ ! -d $candidate ]]; do
    candidate=${candidate%/*}
    [[ -n $candidate ]] || candidate=/
  done
  (cd -- "$candidate" && pwd -P)
}

require_inside() {
  local label=$1
  local resolved

  resolved=$(nearest_directory "$2") || refuse "$label $2"
  [[ $resolved/ == "$root"/* ]] || refuse "$label $resolved"
}

directory=$(pwd -P) || refuse 'an unreadable working directory'
arguments=("$@")
while (($# > 0)); do
  case $1 in
  -C)
    (($# >= 2)) || break
    # Git ignores an empty -C. One it cannot enter is left for Git to
    # reject, which it does before doing anything else.
    if [[ -n $2 ]]; then
      if [[ $2 == /* ]]; then
        directory=$2
      else
        directory="$directory/$2"
      fi
      directory=$(cd -- "$directory" 2>/dev/null && pwd -P) ||
        exec "$real_git" "${arguments[@]}"
    fi
    shift 2
    ;;
  --git-dir | --work-tree)
    (($# >= 2)) || break
    require_inside "$1" "$2"
    shift 2
    ;;
  --git-dir=* | --work-tree=*)
    require_inside "${1%%=*}" "${1#*=}"
    shift
    ;;
  -c | --namespace)
    shift
    (($# == 0)) || shift
    ;;
  -*) shift ;;
  *) break ;;
  esac
done

for variable in GIT_DIR GIT_WORK_TREE GIT_COMMON_DIR GIT_INDEX_FILE \
  GIT_OBJECT_DIRECTORY; do
  [[ -z ${!variable:-} ]] || require_inside "$variable" "${!variable}"
done
[[ $directory/ == "$root"/* ]] || refuse "$directory"

exec "$real_git" "${arguments[@]}"
