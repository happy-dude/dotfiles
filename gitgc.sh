#! /usr/bin/env bash

###################################################################################################
###################################################################################################
### Git compression script for Happy-Dude's dotfiles repository
###
### Author: Stanley Chan
### Github: https://github.com/Happy-Dude/dotfiles.git
### Version: 14 April 2013
###
###################################################################################################
###################################################################################################


# Check if Git is installed update all submodules
`which git` --version 2>&1 >/dev/null # improvement by tripleee
GIT_IS_AVAILABLE=$?

if [ $GIT_IS_AVAILABLE -eq 0 ]; then
    GIT_PATH=`which git`
    GIT_OPTS="submodule foreach --recursive"

    echo "Running git prune"
    $GIT_PATH remote prune origin
    $GIT_PATH $GIT_OPTS $GIT_PATH $GIT_OPTS $GIT_PATH remote prune origin
    $GIT_PATH $GIT_OPTS $GIT_PATH remote prune origin

    echo "Running git gc"
    $GIT_PATH gc
    $GIT_PATH $GIT_OPTS $GIT_PATH $GIT_OPTS $GIT_PATH gc
    $GIT_PATH $GIT_OPTS $GIT_PATH gc

else
    echo "Git not found\; please get latest updates for repository submodules manually"
fi
