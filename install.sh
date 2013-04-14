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

#! /bin/bash

# Check if Git is installed and clone the repository and all submodules
`which git` --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?

if [ $GIT_IS_AVAILABLE -eq 0 ]; then
    GIT_PATH=`which git`
    GIT_OPTS="clone --recursive https://github.com/Happy-Dude/dotfiles.git $HOME/dotfiles"

    echo "Git found: cloning https://github.com/Happy-Dude/dotfiles.git into $HOME/dotfile"
    echo $GIT_PATH $GIT_OPTS
else
    echo "Git not found\nPlease install git or download the zip archive from https://github.com/Happy-Dude/dotfiles.git"
fi

`which vim` --version 2>&1 >/dev/null
VIM_IS_AVAILABLE=$?

if [ $VIM_IS_AVAILABLE -eq 0 ]; then
    echo "Vim found: backing up existing .vimrc configuration and linking dotfiles/vim/vimrc"
fi
