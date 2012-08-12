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


Installation
------------

To install the configuration, just execute the install script (yet to be
written). Since I have not written it yet, I think I'll call it linkify.sh or
something like that... After all, all installation entails is just the linking
of the files to the right places.

Manually install
----------------

To manually install the configurations, first you must check out the repository
using the following command:

    git clone --recursive https://github.com/Happy-Dude/dotfiles.git ~/dotfiles;

Or, to get just my files (without the submodules), just execute

    git clone https://github.com/Happy-Dude/dotfiles.git ~/dotfiles;

folowed by

    cd ~/dotfiles; \
    git submodule init; \
    git submodule update;

After the repository has been downloaded, you can link the different
configurations to their proper locations.

### Vim configuration

    ln -s ~/dotfiles/vim ~/.vim; \
    ln -s ~/dotfiles/vim/vimrc ~/.vimrc; \
    ln -s ~/dotfiles/vim/gvimrc ~/.gvimrc; \

### Tmux configuration

    ln -s ~/dotfiles/tmux.conf ~/.tmux.conf;

Notes
-----

### Compiled Vim settings

For my vim configurations to work properly, it needs to be compiled with various
language support. These features (such as Python and Ruby support) usually are
not included with the default Linux repository versions.

I prefer to do it by checking out the latest Vim source code from Bram
Moolenaar's repository. Make sure that the `~/sources` directory already exists,
if not, make it by executing the command `mkdir ~/sources`.

    hg clone https://code.google.com/p/vim/ ~/sources/vim; \
    cd ~/sources/vim; \
    hg pull; \
    hg update default;

Once the repository is checked out, compile and install it using the following
commands:

    cd ~/sources/vim/src; \
    make distclean; \
    ./configure --with-features=huge --enable-gui=auto --enable-cscope \
    --enable-luainterp --enable-mzschemeinterp --enable-perlinterp \
    --enable-pythoninterp --enable-python3interp --enable-rubyinterp \
    --enable-tclinterp; \

Now that it is configured, compile it, test to see if everything is okay, and
then install it:

    make; \
    make test; \
    make install;

Also, if Vim cannot be installed in /usr/bin, you can try to install it locally
to your user by adding the `--prefix=/usr/local` flag when configuring.

### Adding plugins and submodules

### IPTable configuration

TO DO
-----


For my own use, I also
