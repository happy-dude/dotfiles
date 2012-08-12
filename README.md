---
title: dotfiles
description: Version controlled dotfiles directory
author: Stanley Chan
tags:
colors:
created: Sun 12 Aug 2012
modified: Sun 12 Aug 2012

---

Stan's dotfiles
===============

Version controlled dot configuration files for (currently) vim and tmux.
Zshell and xmonad configurations will be developed and merged in.
Hopefully, I make a conscious effort to also make the configurations cross
platform for Linux, Windows, and OS X.


Installtion
-----------

To install the configuration, just execute the install script (yet to be
written).

All installation entails is just the linking of the files to the right places.

To manually install the configurations, first you must check out the repository
using the following command:

    git clone --recursive https://github.com/Happy-Dude/dotfiles.git ~/dotfiles;

Or, to get just my files (without the submodules), just execute

    git clone https://github.com/Happy-Dude/dotfiles.git ~/dotfiles;

folowed by

    cd ~/dotfiles; \
    git submodule init; \
    git submodule update;

After the repository has been downloaded, execute the following commands:

    ln -s ~/dotfiles/vim ~/.vim; \
    ln -s ~/dotfiles/vim/vimrc ~/.vimrc; \
    ln -s ~/dotfiles/vim/gvimrc ~/.gvimrc; \
    ln -s ~/dotfiles/tmux.conf ~/.tmux.conf;


