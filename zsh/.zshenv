# Install Prezto in $XDG_CONFIG_HOME
# ref: https://github.com/sorin-ionescu/prezto?tab=readme-ov-file#manual
#
# Optionally, if you already have $XDG_CONFIG_HOME configured (usually as
# $HOME/.config by default) and intend to install Prezto under
# $XDG_CONFIG_HOME/zsh instead, you can clone the repository there and configure
# $ZDOTDIR separately if not already configured.

export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:=$HOME/.config}"
[[ -d $XDG_CONFIG_HOME/zsh ]] && export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
source "$ZDOTDIR/.zshenv"
