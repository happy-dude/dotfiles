#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi
if [ -e /Users/stahn_mchan/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/stahn_mchan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
