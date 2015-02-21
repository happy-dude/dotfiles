#! /usr/bin/env bash

###################################################################################################
###################################################################################################
### Install script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: Sat 21 Feb 2015
###
###################################################################################################
###################################################################################################


# Check if Git is installed and clone the repository and all submodules
`which git` --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?

if [ $GIT_IS_AVAILABLE -eq 0 ]; then
    GIT_PATH=`which git`
    GIT_OPTS="clone --recursive https://github.com/Happy-Dude/dotfiles.git $HOME/dotfiles"
    GIT_UPDATE_OPTS="submodule update --init --recursive"
    GIT_SUB_CHECKOUT="submodule foreach $GIT_PATH checkout master"

    echo "Git found: cloning dotfiles repository into $HOME/dotfiles"
    $GIT_PATH $GIT_OPTS
    $GIT_PATH $GIT_SUB_OPTS
    $GIT_PATH $GIT_UPDATE_OPTS
    $GIT_PATH $GIT_SUB_CHECKOUT

else
    echo "Git not found; please install git or download the zip archive of repo"
fi

`which vim` --version 2>&1 >/dev/null
VIM_IS_AVAILABLE=$?

if [ $VIM_IS_AVAILABLE -eq 0 ]; then
    echo "vim found"

    if [ -f $HOME/.vimrc ]; then
        echo "Backing up existing .vimrc file to .vimrc.bak"
        mv $HOME/.vimrc $HOME/.vimrc.bak
    fi

    if [ -d $HOME/.cache ]; then
        if [ -d $HOME/.cache/vim ]; then
            echo "Backing up existing vim cache directory in .cache"
            mv $HOME/.cache/vim $HOME/.cache/vim.bak
        fi
    fi

    echo "Creating vim backup, cache, and swap directories in $HOME/.cache/vim"
    mkdir -p $HOME/.cache/vim $HOME/.cache/vim/backup $HOME/.cache/vim/swap $HOME/.cache/vim/undo

    echo "Linking $HOME/dotfiles/vim to $HOME/.vim"
    ln -s $HOME/dotfiles/vim $HOME/.vim

    echo "Linking $HOME/dotfiles/vim/vimrc to $HOME/.vimrc"
    ln -s $HOME/dotfiles/vim/vimrc $HOME/.vimrc
fi

`which nvim` --version 2>&1 >/dev/null
NVIM_IS_AVAILABLE=$?

if [ $NVIM_IS_AVAILABLE -eq 0 ]; then
    echo "nvim found"

    if [ -f $HOME/.nvimrc ]; then
        echo "Backing up existing .nvimrc file to .nvimrc.bak"
        mv $HOME/.nvimrc $HOME/.nvimrc.bak
    fi

    echo "Linking $HOME/dotfiles/vim to $HOME/.nvim"
    ln -s $HOME/dotfiles/vim $HOME/.nvim

    echo "Linking $HOME/dotfiles/vim/vimrc to $HOME/.nvimrc"
    ln -s $HOME/dotfiles/vim/vimrc $HOME/.nvimrc
fi

`which tmux` -V 2>&1 >/dev/null
TMUX_IS_AVAILABLE=$?

if [ $TMUX_IS_AVAILABLE -eq 0 ]; then
    echo "tmux found"

    if [ -f $HOME/.tmux.conf ]; then
        echo "Backing up existing .tmux.conf file to .tmux.conf.bak"
        mv $HOME/.tmux.conf $HOME/.tmux.conf.bak
    fi

    echo "Linking $HOME/dotfiles/tmux/tmux.conf to $HOME/.tmux.conf"
    ln -s $HOME/dotfiles/tmux/tmux.conf $HOME/.tmux.conf
fi

`which zsh` --version 2>&1 >/dev/null
ZSH_IS_AVAILABLE=$?

if [ $ZSH_IS_AVAILABLE -eq 0 ]; then
    echo "zsh found"

    if [ -f $HOME/.zshrc ]; then
        echo "Backing up existing .zshrc file to .zshrc.bak"
        mv $HOME/.zshrc $HOME/.zshrc.bak
    fi

    echo "Linking Sorin's zsh-prezto"
    ln -s ~/dotfiles/zsh/prezto-sorin ~/.zprezto;
    setopt EXTENDED_GLOB
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
        ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done

    echo "Changing default shell to zsh"
    chsh -s `which zsh`
fi


`which iptables` -V 2>&1 >/dev/null
IPTABLES_IS_AVAILABLE=$?
if [ $IPTABLES_IS_AVAILABLE -eq 0 ]; then
    echo "iptables found"

    if [ -f "/etc/iptables/iptables.rules" ]; then
        echo "Backing up existing IPTables ruleset"
        mv /etc/iptables/iptables.rules /etc/iptables/iptables.rules.bak
    fi

    echo "Linking $HOME/dotfiles/iptables/iptables.rules to /etc/iptables/iptables.rules"
    ln -s $HOME/dotfiles/iptables/iptables.rules /etc/iptables/iptables.rules

    echo "Enabling iptables systemd unit"
    systemctl enable iptables.service
fi

`which ip6tables` -V 2>&1 >/dev/null
IP6TABLES_IS_AVAILABLE=$?
if [ $IP6TABLES_IS_AVAILABLE -eq 0 ]; then
    echo "IP6Tables found"

    if [ -f "/etc/iptables/ip6tables.rules" ]; then
        echo "Backing up existing IP6Tables ruleset"
        mv /etc/iptables/ip6tables.rules /etc/iptables/ip6tables.rules.bak
    fi

    echo "Linking $HOME/dotfiles/iptables/ip6tables.rules to /etc/ip6tables/iptables.rules"
    ln -s $HOME/dotfiles/iptables/ip6tables.rules /etc/ip6tables/iptables.rules

    echo "Enabling ip6tables systemd unit"
    systemctl enable ip6tables.service
fi

`which slim` -v 2>&1 >/dev/null
SLIM_IS_AVAILABLE=$?
if [ $SLIM_IS_AVAILABLE -eq 0 ]; then
    echo "slim found"

    if [ -f "/etc/slim.conf" ]; then
        echo Backing up existing slim configuration"
        mv /etc/slim.conf /etc/slim.conf.bak
    fi

    echo "Linking $HOME/dotfiles/slim/slim.conf to /etc/slim/slim.conf"
    ln -s $HOME/dotfiles/slim/slim.conf /etc/slim/slim.conf

    echo "Enabling slim systemd unit"
    systemctl enable slim.service
fi


