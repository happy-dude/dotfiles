#! /bin/bash

###################################################################################################
###################################################################################################
### Install Script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: 14 April 2013
###
###################################################################################################
###################################################################################################


# Check if Git is installed and clone the repository and all submodules
`which git` --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?

if [ $GIT_IS_AVAILABLE -eq 0 ]; then
    GIT_PATH=`which git`
    GIT_OPTS="clone --recursive https://github.com/Happy-Dude/dotfiles.git $HOME/dotfiles"
    GIT_SUB_OPTS="submodule init"
    GIT_UPDATE_OPTS="submodule update"
    GIT_SUB_CHECKOUT="submodule foreach $GIT_PATH checkout master"

    echo "Git found: cloning dotfiles repository into $HOME/dotfiles"
    echo $GIT_PATH $GIT_OPTS
    echo $GIT_PATH $GIT_SUB_OPTS
    echo $GIT_PATH $GIT_UPDATE_OPTS
    echo $GIT_PATH $GIT_SUB_CHECKOUT

else
    echo "Git not found; please install git or download the zip archive of repo"
fi

`which vim` --version 2>&1 >/dev/null
VIM_IS_AVAILABLE=$?

if [ $VIM_IS_AVAILABLE -eq 0 ]; then
    echo "Vim found"

    if [ -f $HOME/.vimrc ]; then
        echo "Backing up existing .vimrc file to .vimrc.bak"
        echo "mv $HOME/.vimrc $HOME/.vimrc.bak"
    fi

    echo "Linking $HOME/dotfiles/vim to $HOME/.vim"
    echo "ln -s $HOME/dotfiles/vim $HOME/.vim"

    echo "Linking $HOME/dotfiles/vim/vimrc to $HOME/.vimrc"
    echo "ln -s $HOME/dotfiles/vim/vimrc $HOME/.vimrc"
fi

`which tmux` -V 2>&1 >/dev/null
TMUX_IS_AVAILABLE=$?

if [ $TMUX_IS_AVAILABLE -eq 0 ]; then
    echo "Tmux found"

    if [ -f $HOME/.tmux.conf ]; then
        echo "Backing up existing .tmux.conf file to .tmux.conf.bak"
        echo "mv $HOME/.tmux.conf $HOME/.tmux.conf.bak"
    fi
fi
