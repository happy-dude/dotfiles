#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

config_dir=$1
old_generation=${2:-}
legacy_dir=$HOME/.emacs.d

refuse() {
  printf 'Emacs would choose %s before the managed init in %s.\n' "$1" "$config_dir" >&2
  printf 'Quit Emacs, inspect that path, move it to an unused backup name, and retry activation. No legacy files were moved.\n' >&2
  exit 1
}

# Startup loads the extensionless XDG init; bytecode takes precedence over
# the managed source even if user-init-file subsequently names init.el.
[[ ! -f $config_dir/init.elc ]] || refuse "$config_dir/init.elc"

# Even a cache-only .emacs.d shadows XDG. A link to the managed directory
# works, but also makes Emacs try the home-root init and its load suffixes.
if [[ -d $legacy_dir || -L $legacy_dir ]]; then
  # An alias may still be dangling until linkGeneration creates the XDG tree.
  [[ $legacy_dir -ef $config_dir || $(realpath -m -- "$legacy_dir") == "$(realpath -m -- "$config_dir")" ]] || refuse "$legacy_dir"
  for init in "$HOME/.emacs.elc" "$HOME/.emacs.el"; do
    [[ ! -f $init ]] || refuse "$init"
  done
fi

if [[ -e $HOME/.emacs || -L $HOME/.emacs ]]; then
  # linkGeneration retires this particular link. A foreign link to the same
  # store file is not owned by the outgoing generation and will survive it.
  if [[ -n $old_generation && -f $old_generation/home-files/.emacs && -L $HOME/.emacs &&
    $(readlink -- "$HOME/.emacs") == "$(realpath -e -- "$old_generation/home-files")/.emacs" ]]; then
    exit 0
  fi
  # A home-root link to the managed name (not just today's store file)
  # also works when the legacy directory points to its XDG companions.
  if [[ -L $HOME/.emacs && (-d $legacy_dir || -L $legacy_dir) ]]; then
    target=$(readlink -- "$HOME/.emacs")
    [[ $target == /* ]] || target=$HOME/$target
    if [[ ${target##*/} == init.el && $(realpath -m -- "${target%/*}") == "$(realpath -m -- "$config_dir")" ]]; then
      exit 0
    fi
  fi
  refuse "$HOME/.emacs"
fi
