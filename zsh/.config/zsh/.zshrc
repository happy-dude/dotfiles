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
#eval "$($(brew --prefix)/bin/brew shellenv)"

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
export PAGER='less --mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --CLEAR-SCREEN'

# cc flags
# https://gcc.gnu.org/onlinedocs/gcc/Debugging-Options.html
# https://clang.llvm.org/docs/UsersManual.html#diagnostics-enable-everything
# https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html
# https://gcc.gnu.org/onlinedocs/gcc/Instrumentation-Options.html
# https://songdongsheng.github.io/2021/03/21/statically-linked-executable-hardening-with-pie/
if command -v clang &> /dev/null
then
    alias cc='clang \
        -g3 -ggdb3 -glldb \
        -Weverything -pedantic \
        -Wconversion \
        -Wdouble-promotion \
        -Wimplicit-fallthrough \
        -Wmissing-prototypes \
        -fno-omit-frame-pointer \
        -fsanitize=address,undefined \
        -fsanitize-trap=alignment \
        -fstack-clash-protection \
        -fstack-protector-strong \
        -fPIE \
        -fPIC \
        -D_FORTIFY_SOURCE=3 \
        -D_GLIBCXX_ASSERTIONS \
        -Wl,-z,defs,-z,relro,-z,now,-z,noexecstack,-z,noexecheap,-pie'
elif command -v gcc &> /dev/null
then
    alias cc='gcc \
        -g3 -ggdb3 \
        -Wall -Wextra -pedantic \
        -Wconversion \
        -Wdouble-promotion \
        -Wimplicit-fallthrough \
        -Wmissing-prototypes \
        -fno-omit-frame-pointer \
        -fsanitize=address,undefined \
        -fsanitize-trap=alignment \
        -fstack-clash-protection \
        -fstack-protector-strong \
        -ftrivial-auto-var-init=zero \
        -fPIE \
        -fPIC \
        -D_FORTIFY_SOURCE=3 \
        -D_GLIBCXX_ASSERTIONS \
        -Wl,-z,defs,-z,relro,-z,now,-z,noexecstack,-z,noexecheap,-pie'
fi

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

# virtme
alias vmeamd="~/sources/virtme-ng/virtme-run --show-boot-console --show-command --memory 8G --rw --rwdir=$HOME/cf/bpf-lsm --kdir . --mods=auto --net user -a nokaslr"

# LLVM, Xcode SDK
#export LDFLAGS="-L$(brew --prefix)/opt/llvm/lib -Wl,-rpath,$(brew --prefix)/opt/llvm/lib"
#export CPPFLAGS="-I$(brew --prefix)/opt/llvm/include"
#export PATH="$(brew --prefix)/opt/llvm/bin:$PATH"
#export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)

# programming language environments

# docker
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain                  # building the VM may output auth URLs the user needs to click
#export DOCKER_DEFAULT_PLATFORM=linux/amd64     # for Apple Silicon: building the VM only works in a amd64 environment at the moment
#export DOCKER_HOST=unix://$HOME/.docker/desktop/docker.sock          # linux docker-desktop host -- comment if using baseline docker-ce
# go
export PATH="/usr/local/go/bin:$PATH"
export PATH="$(go env GOPATH)/bin:$PATH"
# lua
export PATH="$HOME/.luarocks/bin:$PATH"
# luamake from sumneko
alias luamake="$HOME/sources/lua-language-server/3rd/luamake/luamake"
# node / nvm
#export PATH="$HOME/node_modules/.bin:$PATH"
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                    # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# perl
#source ~/perl5/perlbrew/etc/bashrc
# rust
[[ ! -r "$HOME/.cargo/env" ]] || source "$HOME/.cargo/env"
export PATH="$HOME/.cargo/bin:$PATH"

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
else
    echo "eza could not be found"
fi

export PATH="$PATH:$HOME/.local/bin"
