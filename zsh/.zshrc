# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

# To customize prompt, run `p10k configure` or edit ~/dotfiles/zsh/.p10k.zsh.
[[ ! -f ~/dotfiles/zsh/.p10k.zsh ]] || source ~/dotfiles/zsh/.p10k.zsh

# Customize to your needs...

# git
alias gl="git log --date=relative --pretty='format:%C(blue)%h%C(auto)%d %s %C(8)- %an, %ad' --graph --all"

# emacsclient
alias et='TERM=xterm-256color emacsclient -nw'
alias ef='emacsclient -nc'

# luamake from sumneko
alias luamake="$HOME/sources/lua-language-server/3rd/luamake/luamake"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND="$(which rg) --files --hidden --follow --glob '!.git'"

# nvim default editor
export EDITOR='nvim'
export VISUAL='nvim'

# LLVM, Xcode SDK
export LDFLAGS="-L$(brew --prefix)/opt/llvm/lib -Wl,-rpath,$(brew --prefix)/opt/llvm/lib"
export CPPFLAGS="-I$(brew --prefix)/opt/llvm/include"
export PATH="$(brew --prefix)/opt/llvm/bin:$PATH"
export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)

# programming language environments
source ~/perl5/perlbrew/etc/bashrc
perlbrew use 5.36.0
export PATH="$HOME/.luarocks/bin:$PATH"
export PATH="$HOME/node_modules/.bin:$PATH"
export PATH="$(go env GOPATH)/bin:$PATH"
source "$HOME/.cargo/env"
export PATH="$HOME/.cargo/bin:$PATH"
