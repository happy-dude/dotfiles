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

zshell, xmonad, and bash configurations will be developed soon.

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
    git submodule update --init --recursive;

The --init flag will initialize the submodule repositories and the --recursive
flag will make sure that nested submodules are also initialized and updated.

After the repository has been downloaded, you can link the different
configurations to their proper locations.

### Vim

    ln -s ~/dotfiles/vim ~/.vim; \
    ln -s ~/dotfiles/vim/vimrc ~/.vimrc; \
    ln -s ~/dotfiles/vim/gvimrc ~/.gvimrc;

### Tmux

    ln -s ~/dotfiles/tmux/tmux.conf ~/.tmux.conf;

### Zsh

Basically, my zsh configuration happens to be Sorin Ionescu's prezto repository
(his oh-my-zsh fork). Basically, read his rational and follow the instructions
in his [README](https://github.com/sorin-ionescu/prezto/blob/master/README.md).

For some reason, his command to copy the zsh runcoms files over to the home
directory didn't work, so I executed the following:

    ln -s ~/dotfiles/zsh/prezto-sorin ~/.oh-my-zsh; \
    for rcfile in ~/.oh-my-zsh/runcoms/z{shenv,shrc,login,logout}; do \
        export newrc=$(basename $rcfile); \
        cp -f $rcfile ~/.$newrc;  \
    done; \
    chsh -s /bin/zsh;

Also, if you happen to be running Mac OS X, execute the following:

    sudo chmod ugo-x /usr/libexec/path_helper

### IPTables configuration

***NOTE***

This has only worked on Ubuntu-based Linux distributions. I need
to learn more about the boot process in other distributions to make them work
right. I'm looking into seeing how it works for Arch Linux.

    ln -s ~/dotfiles/iptables /etc/iptables; \
    ln -s ~/dotfiles/iptables/iptables /etc/init.d/iptables; \
    chmod +x /etc/init.d/iptables; \
    sudo update-rc.d iptables

Execute the script with `service iptables start`. Again, this so far has only
been tested on Ubuntu-based distributions.

Branches
--------

There are (currently) two branches of this repository, `master` and `osx`. Mac OS X
users should checkout the osx branch by executing:

    git checkout -b osx origin/osx

Linux users can use the `master` branch without any problems.

### Creating new branches

To add a new branch, use `git branch -b <branch name>`. To add it to the remote
repository, use `git push origin <branch name>`.

If this branch were official, you would also need to add a remote url with

    git remote add <branch name> <repository url>

and verify the changes, use

    git remote -v

Updating
--------

To get new changes into your repository, execute `git fetch`.

To pull those changes in, execute `git pull`.

### Submodules

For the submodules, the proper workflow is to get the repository changes from
the top-level repository. Execute the following:

    cd ~/dotfiles; \
    git pull; \
    git submodule update --recursive;

The above commands would grab the contents of the submodule repositories,
checking out the changes from the commit entry in the `.gitmodules` file.

To further update the submodules beyond what the top-level repository/ branch
has, go into the repository and issue a git pull to get the changes you want or
checkout the branches you need, which also modifies the `.gitmodules` file.
If you wish to incorporate those changes into the top-level branch, go back
into the top level directory and `commit`.

    cd <submodule directory>; \
    git pull <branch, commit, rebase, whatever>; \
    cd ~/dotfiles; \
    git commit;

If you want to update all the submodules, you can use the following
commands:

    git submodule foreach --recursive git checkout master; \
    git submodule foreach --recursive git pull;

Since the changes to the `.gitmodules` are commited, if you want them in
another computer, just do a `pull` in the top level directory and then
`update`:

    cd ~/dotfiles; \
    git pull; \
    git submodule update --recursive;

There is not a way in Git (yet?) that can do a pull from the top-level
repository and also update the submodules; you have to do them separately.

***TIP***

If there are changes inside the submodules, use due dilligence in reading about
the changes and patches before actually incorporating them into your
configuration; that got me once or twice...

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

### tmux and Mac OS X

Installing tmux on OS X on using Homebrew (and likely Macports also) would cause
a message saying `launch_msg("SetUserEnvironment"): Socket is not connected`
error. According to a
[StackOverflow](http://stackoverflow.com/questions/10193561/tmux-socket-is-not-connected-error-on-os-x-lion)
answer, and ChrisJohnsen's
[tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard),
it seems that OS X's pbcopy and pbpaste "fail to function properly" for
applicationsthat run on Terminal emulators and have clipboard access (like vim,
tmux, and screen).

Chris Johnsen's patch should fix this problem. Install his
reattach-to-user-namespace wrapper/patch using the following command (if you're
using Homebrew):

    brew install reattach-to-user-namespace

If you are using Macports, execute

    port install tmux-pasteboard

### Vim and Tmux terminal colors

Colored terminal applications, like vim, tmux, and screen, need the proper
settings enabled to have colored output. These settings usually are found in your
`zshrc`, `bashrc`, or whatever shell-rc/init file you have.

In those files, the `$TERM` variable should be set to either `xterm-256color` or
`screen-256color` depending on your environment. Also remember that these settings
are only meanigful if have the proper `terminfo` files (located in
/usr/share/terminfo/ directy in Ubuntu-based Linux -- I will check out how is in
on Arch and OS X on a later date).

Your <shell>rc files should have one of these following settings:

    export TERM='xterm-256color'

or for tmux/screen environments:

    export TERM='screen-256color'

### Adding plugins and submodules

It's best to add plugins to the repository and let Tim Pope's awesome
[fugitive.vim](https://github.com/tpope/vim-fugitive) take care of the vim
runtime path.

Since fugitive is already installed with the vim configurations, just add it via
a submodule by executing `git submodule add <git url> <directory name>` inside
the `dotfiles` directory. You can also use repositories that are managed by other
version control systems, but I personally have not experimented with that much.

### Zsh configuration

"My" zsh configuration is simply Sorin Ionescu's
[prezto](https://github.com/sorin-ionescu/prezto) repository, which is in turn a
fork of Robby Russell's popular
[oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) repository. Basically,
both are a community-driven configuration of zsh that happens to add a lot of
useful to the shell environment.

I choose Sorin's fork because he has made extensive changes to the origin Rob
Russel version that were not merged into the master branch. More of this is
discussed in [Issue #337](https://github.com/robbyrussell/oh-my-zsh/issues/377)
in oh-my-zsh's issue tracker. The series of fixes and bugs are highly appreciated
and thus I settled with his version.

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
