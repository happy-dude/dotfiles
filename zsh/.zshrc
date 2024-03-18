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

# Turn off terminal flow control (ctrl-q and ctrl-s)
# already set in prezto with `unsetopt FLOW_CONTROL` in modules/completion/init.zsh
#stty -F/dev/tty -ixon -ixoff

# LESS mouse scrolling
export LESS='--mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --window=-4 --CLEAR-SCREEN'

# cc flags
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
        -fstack-protector-all \
        -fPIE \
        -fPIC \
        -D_FORTIFY_SOURCE=2 \
        -Wl,-z,relro,-z,now,-z,noexecstack,-z,noexecheap,-pie'
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
        -fstack-protector-all \
        -fPIE \
        -fPIC \
        -D_FORTIFY_SOURCE=2 \
        -Wl,-z,relro,-z,now,-z,noexecstack,-z,noexecheap,-pie'
fi

# virtme
alias vmeamd="~/sources/virtme-ng/virtme-run --show-boot-console --show-command --memory 8G --rw --rwdir=/home/schan/cf-repos/bpf-lsm --kdir . --mods=auto --net user -a nokaslr"

# git
alias gl="git log --date=relative --abbrev=12 -n 160 \
    --pretty='format:%C(dim blue)%h%C(auto)%d %s %>|(68,trunc)%C(8)- %C(dim magenta)%an%C(8), %ad' --graph --all"
alias gits="git --no-pager show --no-patch --format='commit %h (\"%s\")%n'"

# emacsclient
alias et='TERM=xterm-256color emacsclient -nw'
alias ef='emacsclient -nc'

# luamake from sumneko
alias luamake="$HOME/sources/lua-language-server/3rd/luamake/luamake"

# fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#[ -d $HOME/dotfiles/vim/.vim/pack/plugged/opt/fzf/shell/ ] && source $HOME/dotfiles/vim/.vim/pack/plugged/opt/fzf/shell/*.zsh
export FZF_DEFAULT_COMMAND="$(which rg) --files --hidden --follow --glob '!.git'"

# nvim default editor
export EDITOR='nvim'
export VISUAL='nvim'

# LLVM, Xcode SDK
#export LDFLAGS="-L$(brew --prefix)/opt/llvm/lib -Wl,-rpath,$(brew --prefix)/opt/llvm/lib"
#export CPPFLAGS="-I$(brew --prefix)/opt/llvm/include"
#export PATH="$(brew --prefix)/opt/llvm/bin:$PATH"
#export SDKROOT=$(xcrun --sdk macosx --show-sdk-path)

# programming language environments
source ~/perl5/perlbrew/etc/bashrc
perlbrew use 5.38.0
export PATH="$HOME/.luarocks/bin:$PATH"
export PATH="$HOME/node_modules/.bin:$PATH"
export PATH="/usr/local/go/bin:$PATH"
export PATH="$(go env GOPATH)/bin:$PATH"
source "$HOME/.cargo/env"
export PATH="$HOME/.cargo/bin:$PATH"
# docker
export DOCKER_BUILDKIT=1
export BUILDKIT_PROGRESS=plain                  # building the VM may output auth URLs the user needs to click
#export DOCKER_DEFAULT_PLATFORM=linux/amd64     # for Apple Silicon: building the VM only works in a amd64 environment at the moment
#export DOCKER_HOST=unix:///home/schan/.docker/desktop/docker.sock          # linux docker-desktop host -- comment if using baseline docker-ce

# eza
if command -v eza &> /dev/null
then
    alias ls='eza'                                                          # ls
    alias l='eza -lbF --git'                                                # list, size, type, git
    alias ll='eza -lbGF --git'                                              # long list
    alias llm='eza -lbGd --git --sort=modified'                             # long list, modified date sort
    alias la='eza -lbhHigUmuSa --time-style=long-iso --git --color-scale'   # all list
    alias lx='eza -lbhHigUmuSa@ --time-style=long-iso --git --color-scale'  # all + extended list

    # specialty views
    alias lS='eza -1'                                                       # one column, just names
    alias lt='eza -lbGF --tree --level=2'                                   # tree
    alias lg='eza -lbGd --git --sort=modified --tree --level=2'             # tree w/ git
else
    echo "eza could not be found"
    exit
fi
# Created by `pipx` on 2023-06-09 20:07:02
#export PATH="$PATH:/Users/stahn_mchan/.local/bin"
export PATH="$PATH:/home/schan/.local/bin"
