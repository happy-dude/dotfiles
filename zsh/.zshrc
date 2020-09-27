#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...
export EDITOR='nvim'
export VISUAL='nvim'

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND="$(which rg) --files --hidden --follow --glob '!.git'"

export PATH="$HOME/.cargo/bin:$PATH"
export PATH=$PATH:$(go env GOPATH)/bin
export PATH="$HOME/.luarocks/bin:$PATH"

