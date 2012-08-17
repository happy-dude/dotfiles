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

Version controlled dot configuration files for (currently) vim, tmux, and iptables.

Zshell, Xmonad, and Bash configurations will be developed soon.

Hopefully, I make a conscious effort to also make the configurations cross
platform for Linux, Windows, and OS X.

Installation
------------

To install the configuration, just execute the install script (yet to be
written).

Since I have not written it yet, I think I'll call it `linkify.sh` or
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
    ln -s ~/dotfiles/vim/gvimrc ~/.gvimrc;

### Tmux configuration

    ln -s ~/dotfiles/tmux/tmux.conf ~/.tmux.conf;

### IPTables configuration

***NOTE***

This has only worked on Ubuntu-based Linux distributions. I need
to learn more about the boot process in other distributions to make them work
right. I'm looking into seeing how it works for Arch Linux.

    ln -s ~/dotfiles/iptables /etc/iptables; \
    ln -s ~/dotfiles/iptables/iptables /etc/init.d/iptables;
    chmod +x /etc/init.d/iptables
    sudo update-rc.d iptables

Execute the script with `service iptables start`. Again, this so far has only
been tested on Ubuntu-based distributions.

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
    --enable-tclinterp;

Also note that --enable-python interp assumes that /usr/bin/python is aliased to
python2, NOT python3 (which is what --enable-python3interp uses). Make sure to
alias /usr/bin/python properly or else Vim will not compile with python2 support.
(I am aware this is an issue in Arch Linux distributions, but I am not sure
where else.)

Now that it is configured, compile it, test to see if everything is okay, and
then install it:

    make; \
    make test; \
    make install;

Also, if Vim cannot be installed in /usr/bin, you can try to install it locally
to your user by adding the `--prefix=/usr/local` flag when configuring.

### Adding plugins and submodules

It's best to add plugins to the repository and let Tim Pope's awesome
[fugitive.vim](https://github.com/tpope/vim-fugitive) take care of the vim
runtime path.

Since fugitive is already installed with the vim configurations, just add it via
a submodule by executing `git submodule add <git url> <directory name>` inside
the `dotfiles` directory. You can also use repositories that are managed by other
version control systems, but I personally have not experimented with that much.

### IPTables configuration

I decided to include my IPTables configuration with this repository, located in
the `dotfiles/iptables` directory.

Inside this directory are

* iptables, the executable script
* iptables.complete, the full list of iptable rules I have made and exported
* iptables.current, the list of iptable rules after importing iptables.complete
* iptables.conf, a configuration file used by the executable script

In Ubuntu, the entire iptables directory should be symlinked into /etc/iptables
and the `iptables` script should be symlinked to `/etc/init.d/iptables`. Once
this is done, you can start and stop your IPTable rules like any other running
service by issuing commands such as `service iptables start` or `service iptables
stop`.

On Arch Linux, this seems to be a little bit different due to how Arch decides
to simply some of the configuration and boot process. I have not implemented the
scripts on a Arch Linux installtion yet, so I have to do a little bit of reading
about the [boot process](https://wiki.archlinux.org/index.php/Arch_Boot_Process).

TO DO
-----

* Make a bash installation script for the files; makes things much easier
* Fix up IPTable configuration and script; make it work with Arch Linux
* Learn more markdown and fix up this README
* Create a proper zshell configuration; see how far I can go with vi keybindings
instead of emacs ones
* Learn Zshell and get a nice oh-my-zsh setup working
* Learn Xmonad
* Learn Arch Linux
* Make configuration files platform independent
* Add more dotfile configurations

LICENSE
-------

Happy-Dude/dotfiles repository (c) by Stanley Chan

[![Creative Commons License](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)](http://creativecommons.org/licenses/by-sa/3.0/)

This work is licensed under the [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/). 

You should have received a copy of the license along with this
work. To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send
a letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.
