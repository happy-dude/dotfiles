#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Add nix completions to fpath
if [[ -d "$HOME/.nix-profile/share/zsh/site-functions" ]]; then
  fpath=("$HOME/.nix-profile/share/zsh/site-functions" $fpath)
fi

if [[ -d "${XDG_CONFIG_HOME:-$HOME/.config}/zsh/completions" ]]; then
  fpath=("${XDG_CONFIG_HOME:-$HOME/.config}/zsh/completions" $fpath)
fi

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# To customize prompt, run `p10k configure` or edit ~/dotfiles/zsh/.config/zsh/.p10k.zsh.
[[ ! -f "${ZDOTDIR:-$HOME}/.p10k.zsh" ]] || source "${ZDOTDIR:-$HOME}/.p10k.zsh"

# History toggle: `nohist` stops persisting this session's commands to disk (handy
# for routine/secret commands you don't want recorded). `yeshist` restores saving.
# In-memory history still works either way, so up-arrow recall is unaffected.
#
# Do NOT implement this by unsetting HISTFILE: prezto sets SHARE_HISTORY, so commands
# typed while "off" stay in the in-memory list and get flushed to disk the moment
# HISTFILE is restored (yeshist) or the shell exits — they leak. Instead a
# zshaddhistory hook drops lines from the *saved* history while the toggle is on, so
# they never reach the file even after yeshist. Registered via add-zsh-hook so it
# stacks with (rather than clobbers) any other zshaddhistory hook.
typeset -g _NOHIST=0
nohist()  { _NOHIST=1; }
yeshist() { _NOHIST=0; }
autoload -Uz add-zsh-hook
_nohist_addhistory() { (( _NOHIST )) && return 1; return 0; }
add-zsh-hook zshaddhistory _nohist_addhistory

# Powerlevel10k: show a magenta-on-black `no-hist` block while history saving is off.
# Kept here rather than in .p10k.zsh so re-running `p10k configure` won't clobber it.
# p10k reads its config lazily on first prompt (after this file runs), so defining the
# segment and appending to the elements array here is enough — no `p10k reload` needed.
function prompt_nohist() {
  (( _NOHIST )) || return
  p10k segment -b 5 -f 0 -i '󰋗' -t 'no-hist'
}
function instant_prompt_nohist() { prompt_nohist }
typeset -g POWERLEVEL9K_NOHIST_BACKGROUND=5   # magenta
typeset -g POWERLEVEL9K_NOHIST_FOREGROUND=0   # black
# Prepend the segment to whatever .p10k.zsh configured (guard against re-source dupes).
if (( ! ${POWERLEVEL9K_LEFT_PROMPT_ELEMENTS[(I)nohist]} )); then
  POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(nohist $POWERLEVEL9K_LEFT_PROMPT_ELEMENTS)
fi

# Nix home-manager
if [[ -s "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]]; then
    source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh
fi

# homebrew
eval "$($(brew --prefix)/bin/brew shellenv)"

# Homebrew curl
export PATH="/opt/homebrew/opt/curl/bin:$PATH"

# Turn off terminal flow control (ctrl-q and ctrl-s)
# already set in prezto with `unsetopt FLOW_CONTROL` in modules/completion/init.zsh
#stty -F/dev/tty -ixon -ixoff   Linux
#stty -ixon -ixoff              macOS

# nvim default editor
export EDITOR='nvim'
export VISUAL='nvim'

# use neovim as manpager
export MANPAGER='nvim +Man!'
export MANWIDTH=80

# LESS mouse scrolling
export LESS='--mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --CLEAR-SCREEN'
export PAGER='less'

# Hardened C compiler wrapper for small standalone builds.
function cc() {
    local compiler
    local -a flags
    local link=1
    local pic=0

    if (( $+commands[clang] )); then
        compiler=clang
        flags=(-O1 -g3 -glldb)
    elif (( $+commands[gcc] )); then
        compiler=gcc
        flags=(-O1 -g3 -ggdb3 -ftrivial-auto-var-init=zero)
    else
        print -u2 'cc: neither clang nor gcc is available'
        return 127
    fi

    flags+=(
        -Wall -Wextra -Wpedantic
        -Wconversion -Wdouble-promotion
        -Wformat=2 -Wimplicit-fallthrough -Wmissing-prototypes
        -fno-omit-frame-pointer
        -fsanitize=address,undefined
        -fstack-clash-protection -fstack-protector-strong
        -D_FORTIFY_SOURCE=3
    )

    local arg
    for arg in "$@"; do
        case "$arg" in
            -c|-E|-S|-fsyntax-only|-M|-MM) link=0 ;;
            -shared|-fPIC) pic=1 ;;
        esac
    done

    if (( pic )); then
        flags+=(-fPIC)
    else
        flags+=(-fPIE)
    fi

    if (( link )); then
        flags+=(
            -Wl,-z,relro -Wl,-z,now -Wl,-z,noexecstack
        )
        (( pic )) || flags+=(-Wl,-z,defs -pie)
    fi

    command "$compiler" "${flags[@]}" "$@"
}

# git
alias gl="git log --date=relative --abbrev=12 -n 160 \
    --pretty='format:%C(dim blue)%h%C(auto)%d %s %>|(68,trunc)%C(8)- %C(dim magenta)%an%C(8), %ad' --graph --all"
alias gits="git --no-pager show --no-patch --format='commit %h (\"%s\")%n'"

# fzf
if command -v fzf &> /dev/null; then
    source <(fzf --zsh)
fi
if command -v rg &> /dev/null; then
    export FZF_DEFAULT_COMMAND="$(command -v rg) --files --hidden --follow --glob '!.git'"
fi


# emacsclient
alias et='TERM=xterm-256color emacsclient -nw'
alias ef='emacsclient -nc'

# LLVM, Xcode SDK
export LDFLAGS="-L$(brew --prefix)/opt/llvm/lib -Wl,-rpath,$(brew --prefix)/opt/llvm/lib"
export CPPFLAGS="-I$(brew --prefix)/opt/llvm/include"
export PATH="$(brew --prefix)/opt/llvm/bin:$PATH"
export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)
#
# Run the current kernel tree with the usual AMD debugging defaults.
function vmeamd() {
    command vng \
        --run \
        --memory 8G \
        --rw \
        --network user \
        --append nokaslr \
        "$@"
}

# programming language environments

# docker
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain                  # building the VM may output auth URLs the user needs to click
# node / nvm
#export PATH="$HOME/node_modules/.bin:$PATH"
# on macOS, node is installed by homebrew instead of nvm
export NVM_DIR="$HOME/.nvm"
if [[ -s "$NVM_DIR/nvm.sh" ]]; then
    function nvm() {
        unfunction nvm
        source "$NVM_DIR/nvm.sh"
        nvm "$@"
    }
fi
[[ ! -s "$NVM_DIR/bash_completion" ]] || source "$NVM_DIR/bash_completion"
# perl
#source ~/perl5/perlbrew/etc/bashrc
# rust
source "$HOME/.cargo/env"
export PATH="$HOME/.cargo/bin:$PATH"

#
# eza
if command -v eza &> /dev/null
then
    alias ls='eza'                                                          # ls
    alias l='eza -lahbF --git'                                              # list, size, type, git
    alias ll='eza -labGF --git'                                             # long list
    alias lla='eza -labhHigUmuS --time-style=long-iso --git --color-scale'  # all list
    alias llm='eza -labGd --git --sort=modified'                            # long list, modified date sort
    alias lx='eza -labhHigUmuS@ --time-style=long-iso --git --color-scale'  # all + extended list

    # specialty views
    alias lS='eza -a1'                                                      # one column, just names
    alias lg='eza -labGd --git --sort=modified --tree --level=2'            # tree w/ git
    alias lt='eza -labGF --tree --level=2'                                  # tree
fi
