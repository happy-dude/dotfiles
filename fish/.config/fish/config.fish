# Nix home-manager
# done automatically in fish config
#set -q __fish_home_manager_config_sourced; and exit
#set -g __fish_home_manager_config_sourced 1
if status is-interactive && tty -s
    set -gx GPG_TTY (tty) # GPG pinentry needs the active terminal.
end

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
fish_add_path -p /opt/homebrew/opt/curl/bin

# Turn off terminal flow control (ctrl-q and ctrl-s)
# already default by off in fish
#stty -F/dev/tty -ixon -ixoff   Linux
#stty -ixon -ixoff              macOS

# Confirm Ctrl-D before an empty interactive shell exits its tmux pane.
function __confirm_tmux_exit
    if set -q TMUX; and test (string length -- (commandline)) -eq 0
        read --local --prompt-str "Exit this shell? [y/N] " response
        commandline --function repaint
        string match --quiet --regex --ignore-case '^y(es)?$' -- "$response"
        and commandline --function exit
        return
    end

    commandline --function exit
end

# ctrl-x ctrl-e to open $EDITOR, like in zsh
if status is-interactive
    bind ctrl-d __confirm_tmux_exit
    bind \cx\ce edit_command_buffer
end

# History toggle: `nohist` selects Fish's private, unsaved history session.
# `yeshist` restores the previous session, including the default session.
function nohist
    set -q fish_history; and test -z "$fish_history"; and return
    if set -q fish_history
        set -g _nohist_previous_fish_history $fish_history
    else
        set -e _nohist_previous_fish_history
    end
    set -g fish_history ''
    echo 'history off'
end

function yeshist
    if set -q _nohist_previous_fish_history
        set -g fish_history $_nohist_previous_fish_history
        set -e _nohist_previous_fish_history
    else
        set -e fish_history
    end
    echo 'history on'
end

# Home Manager supplies the prompt for interactive sessions.
if status is-interactive
    source (status dirname)/tide.fish
end

# Hardened C compiler wrapper for small standalone builds.
function c
    set -l compiler
    set -l flags
    if command -q clang
        set compiler clang
        set flags -O2 -g3 -glldb
    else if command -q gcc
        set compiler gcc
        set flags -O2 -g3 -ggdb3 -ftrivial-auto-var-init=zero
    else
        echo 'c: neither clang nor gcc is available' >&2
        return 127
    end

    set -a flags \
        -Wall -Wextra -Wpedantic \
        -Wconversion -Wdouble-promotion \
        -Wformat=2 -Wimplicit-fallthrough -Wmissing-prototypes \
        -fno-omit-frame-pointer \
        -fsanitize=address,undefined \
        -fstack-clash-protection -fstack-protector-strong \
        -D_FORTIFY_SOURCE=3

    set -l link 1
    set -l pic 0
    for arg in $argv
        switch $arg
            case -c -E -S -fsyntax-only -M -MM
                set link 0
            case -shared -fPIC
                set pic 1
        end
    end

    if test $pic -eq 1
        set -a flags -fPIC
    else
        set -a flags -fPIE
    end

    if test $link -eq 1
        set -a flags \
            -Wl,-z,relro -Wl,-z,now -Wl,-z,noexecstack
        if test $pic -eq 0
            set -a flags -Wl,-z,defs -pie
        end
    end

    command $compiler $flags $argv
end

## git
alias gl="git log --date=relative --abbrev=12 -n 160 \
    --pretty='format:%C(dim blue)%h%C(auto)%d %s %>|(68,trunc)%C(8)- %C(dim magenta)%an%C(8), %ad' --graph --all"
alias gits="git --no-pager show --no-patch --format='commit %h (\"%s\")%n'"

# emacsclient
# Attach terminal clients; detach GUI and Org capture clients.
# An empty alternate editor starts the daemon if the user service is not ready.
alias et='emacsclient --alternate-editor= --tty'
alias ef='emacsclient --alternate-editor= --create-frame --no-wait'
alias ec="emacsclient --alternate-editor= --no-wait --eval '(make-capture-frame)'"

# Ubuntu/Fedora system libs (for Nix gcc/ld to find distro-installed libraries)
set -gx LIBRARY_PATH "/usr/lib/x86_64-linux-gnu:/usr/lib64"

# LLVM, Xcode SDK
set -gx LDFLAGS "-L$(brew --prefix)/opt/llvm/lib -Wl,-rpath,$(brew --prefix)/opt/llvm/lib"
set -gx CPPFLAGS "-I$(brew --prefix)/opt/llvm/include"
fish_add_path -p "$(brew --prefix)/opt/llvm/bin"
set -gx SDKROOT $(xcrun --sdk macosx --show-sdk-path)

# Run the current kernel tree with the usual AMD debugging defaults.
function vmeamd --wraps vng
    command vng \
        --run \
        --memory 8G \
        --rw \
        --network user \
        --append nokaslr \
        $argv
end

# programming language environments

# eza
if command -q eza
    alias ls='eza' # ls
    alias l='eza -lahbF --git' # list, size, type, git
    alias ll='eza -labGF --git' # long list
    alias llm='eza -labGd --git --sort=modified' # long list, modified date sort
    alias lla='eza -labhHigUmuS --time-style=long-iso --git --color-scale' # all list
    alias lx='eza -labhHigUmuS@ --time-style=long-iso --git --color-scale' # all + extended list

    # specialty views
    alias lS='eza -a1' # one column, just names
    alias lt='eza -labGF --tree --level=2' # tree
    alias lg='eza -labGd --git --sort=modified --tree --level=2' # tree w/ git
end

fish_add_path -a "$HOME/.local/bin"

# Machine-local secrets stay outside Git and the Nix store.
if test -r "$HOME/.config/fish/secrets.fish"
    source "$HOME/.config/fish/secrets.fish"
end
