# Nix home-manager
# done automatically in fish config
#set -q __fish_home_manager_config_sourced; and exit
#set -g __fish_home_manager_config_sourced 1
set -gx GPG_TTY (tty) #nix gpg pinentry workaround

#fish_add_path -p "/nix/var/nix/profiles/default/bin"

# homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"
if test -d (brew --prefix)"/share/fish/completions"
    set -p fish_complete_path (brew --prefix)/share/fish/completions
end

if test -d (brew --prefix)"/share/fish/vendor_completions.d"
    set -p fish_complete_path (brew --prefix)/share/fish/vendor_completions.d
end

# Homebrew curl
fish_add_path -p "/opt/homebrew/opt/curl/bin"

# ghostty
# ghostty requires OpenGL 3.3, which UTM 4.x unfortunately does not support (yet?)
# workaround: use `LIBGL_ALWAYS_SOFTWARE=true ghostty` alias
# refs:
#   https://github.com/ghostty-org/ghostty/discussions/2602
#   https://github.com/utmapp/UTM/issues/4285
#   https://docs.mesa3d.org/envvars.html#envvar-LIBGL_ALWAYS_SOFTWARE
#function ghostty
#    set -lx LIBGL_ALWAYS_SOFTWARE true
#    command ghostty $argv
#end

# Turn off terminal flow control (ctrl-q and ctrl-s)
# already default by off in fish
#stty -F/dev/tty -ixon -ixoff   Linux
#stty -ixon -ixoff              macOS

# nvim default editor
set -gx EDITOR 'nvim'
set -gx VISUAL 'nvim'

# ctrl-x ctrl-e to open $EDITOR, like in zsh
bind \cx\ce edit_command_buffer

# History toggle: `nohist` switches to a private, unsaved history session by
# clearing the $fish_history session name (handy for routine/secret commands you
# don't want recorded) — exactly what `fish --private` does from launch. Because
# this swaps the whole session (not just disable saving), commands typed while off
# are recallable until you run `yeshist`, but prior "fish"-session history isn't
# visible during that window. `yeshist` restores the saved "fish" session; commands
# from the off window are then dropped, never hitting disk. The tide prompt shows a
# magenta `no-hist` block while off.
#
# These export (`-gx`, not just `-g`) on purpose: tide computes the prompt in a
# forked `fish -c` subprocess, which only inherits *exported* variables. A plain
# global would toggle history correctly but stay invisible to that subprocess, so
# the `_tide_item_nohist` block below would never render. Exporting fixes that.
function nohist;  set -gx fish_history '';   echo 'history off'; end
function yeshist; set -gx fish_history fish; echo 'history on';  end

# tide item: render a magenta-on-black warning while history saving is off.
function _tide_item_nohist
    # Only warn when fish_history is *explicitly* set to empty (via nohist). By
    # default the variable is unset (history on), which `test -z` alone would
    # wrongly flag as off, showing no-hist on every fresh shell.
    set -q fish_history; and test -z "$fish_history"; or return
    _tide_print_item nohist '󰋗 no-hist'
end
set -g tide_nohist_bg_color magenta
set -g tide_nohist_color black
set -g tide_left_prompt_items nohist $tide_left_prompt_items

# use neovim as manpager
set -gx MANPAGER 'nvim +Man!'
set -gx MANWIDTH 80

## LESS mouse scrolling
set -gx LESS '--mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --CLEAR-SCREEN'
set -gx PAGER 'less --mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --CLEAR-SCREEN'

## cc flags
## https://gcc.gnu.org/onlinedocs/gcc/Debugging-Options.html
## https://clang.llvm.org/docs/UsersManual.html#diagnostics-enable-everything
## https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html
## https://gcc.gnu.org/onlinedocs/gcc/Instrumentation-Options.html
## https://songdongsheng.github.io/2021/03/21/statically-linked-executable-hardening-with-pie/
if command -v clang &> /dev/null
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
else if command -v gcc &> /dev/null
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
end

## git
alias gl="git log --date=relative --abbrev=12 -n 160 \
    --pretty='format:%C(dim blue)%h%C(auto)%d %s %>|(68,trunc)%C(8)- %C(dim magenta)%an%C(8), %ad' --graph --all"
alias gits="git --no-pager show --no-patch --format='commit %h (\"%s\")%n'"

# fzf
#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#[ -d $HOME/dotfiles/vim/.vim/pack/plugged/opt/fzf/shell/ ] && source $HOME/dotfiles/vim/.vim/pack/plugged/opt/fzf/shell/completion.zsh && source $HOME/dotfiles/vim/.vim/pack/plugged/opt/fzf/shell/key-bindings.zsh
if test -d $HOME/dotfiles/vim/.vim/pack/plugged/opt/fzf/shell
    fzf --fish | source
    if command -v rg &> /dev/null
        set -gx FZF_DEFAULT_COMMAND "$(which rg) --files --hidden --follow --glob '!.git'"
    end
end

# emacsclient
alias et='TERM=xterm-256color emacsclient -nw'
alias ef='emacsclient -nc'

## virtme
alias vmeamd="~/sources/virtme-ng/virtme-run --show-boot-console --show-command --memory 8G --rw --rwdir=$HOME/cf/bpf-lsm --kdir . --mods=auto --net user -a nokaslr"

# Ubuntu/Fedora system libs (for Nix gcc/ld to find distro-installed libraries)
set -gx LIBRARY_PATH "/usr/lib/x86_64-linux-gnu:/usr/lib64"

# LLVM, Xcode SDK
set -gx LDFLAGS "-L$(brew --prefix)/opt/llvm/lib -Wl,-rpath,$(brew --prefix)/opt/llvm/lib"
set -gx CPPFLAGS "-I$(brew --prefix)/opt/llvm/include"
fish_add_path -p "$(brew --prefix)/opt/llvm/bin"
set -gx SDKROOT $(xcrun --sdk macosx --show-sdk-path)

# programming language environments

# docker
set -gx DOCKER_BUILDKIT 1
set -gx BUILDKIT_PROGRESS plain                  # building the VM may output auth URLs the user needs to click
#set -gx DOCKER_DEFAULT_PLATFORM linux/amd64     # for Apple Silicon: building the VM only works in a amd64 environment at the moment
#set -gx DOCKER_HOST unix://$HOME/.docker/desktop/docker.sock          # linux docker-desktop host -- comment if using baseline docker-ce
# go
fish_add_path -p "/usr/local/go/bin"
fish_add_path -p "$(go env GOPATH)/bin"
# lua
fish_add_path "$HOME/.luarocks/bin"
# luamake from sumneko
alias luamake="$HOME/sources/lua-language-server/3rd/luamake/luamake"
# node / nvm
set -gx NVM_DIR "$HOME/.nvm"
# nvm scripts not compatible with non-POSIX fish, use nvm.fish plugin
set --universal nvm_default_version system
# perl
#source ~/perl5/perlbrew/etc/bashrc
# rust
fish_add_path -p "$HOME/.cargo/bin"
fish_add_path -p "$HOME/.rustowl"

# eza
if command -v eza &> /dev/null
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
end

fish_add_path -a "$HOME/.local/bin"
