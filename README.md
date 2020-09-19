# Happy-Dude's dotfiles

> happy-dude's personal dotfiles repo

This repo is managed solely by myself and configured for my personal workflows
and use-cases.

Feel free to browse and adopt any settings to help tweak your own setup; I've
done my best to attribute resources in the comments for further details.

My entire workflow is currently based within macOS, reflecting in Unix-compatible
configurations. I have not used Linux nor Windows as a daily driver in a while and
I will make the appropriate changes to make my settings compatible when I do.

## Branches

Multiple branches of dotfiles are useful for different platforms or environments
that require modified settings.

There are (currently) two branches, `master` and `macos`. macOS users should
checkout the `macos` branch by executing:

```bash
git checkout -b macos origin/macos
```

## Submodules

This repo contains git [submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
for many [configs](https://github.com/Happy-Dude/dotfiles/tree/master/zsh),
[vim](https://github.com/Happy-Dude/dotfiles/tree/master/vim/.vim/pack/bundle/opt)
, and
[emacs](https://github.com/Happy-Dude/dotfiles/tree/master/emacs/.config/emacs/plugins)
plugin packages.

Each of these project repos have their own maintainers, licenses, and issue
trackers. Credit goes to each respective project and their communities for
providing useful tool for people like me to use. Please check them out and
support them where you can!

## Tweaking and Tuning

I recommend starting with minimal configs and adopting tweaks over time
that:

1. streamlines a workflow you perform regularly or
2. helps overcome and solve a problem at hand.

Approaching your own tweaking and tuning with these points in mind help make
settings easy to understand and immediately productive while saving on bloat
from superfluous settings you may not actually need.

## Install

TODO: write a script that "stowifies" the configs automatically. Take a look at
[prior-art](https://writingco.de/blog/how-i-manage-my-dotfiles-using-gnu-stow/#automate-the-dotfiles)
and remix, transform, and adapt accordingly.

### Requirements

* [git](https://github.com/git/git)
* [gnu stow](https://github.com/aspiers/stow/)
* [bash](https://www.gnu.org/software/bash/) (for scripts)

1. Clone the repository and submodules

    ```bash
    git clone --recursive https://github.com/Happy-Dude/dotfiles.git $HOME/dotfiles
    cd $HOME/dotfiles
    git submodule update --init --recursive --remote
    ```

2. Use GNU Stow to symlink configs

    NOTE: stow will not symlink over an existing file or symlink. Check out the
    [manual](https://www.gnu.org/software/stow/manual/stow.html#Conflicts) for more
    details.

    ```bash
    cd $HOME/dotfiles
    stow vim
    stow tmux
    stow zsh
    ... etc ...
    ```

### Neovim

Unfortunately, stow does not [currently](https://github.com/aspiers/stow/issues/3#issuecomment-586654099)
handle directories or files that are symlinked. For example, symlinking
`neovim/init.vim` to `vim/vimrc` and then running `stow` on
the directories do not have the effect of creating a symlink to the absolute path
of the original link, `neovim/init.vim`.

Since I have my neovim and vim configurations backwards-compatible with each other
and using the same directories, I use stow for the base vim configs and use the
following symlink for neovim:

```bash
ln -s $HOME/dotfiles/.vim $HOME/.config/nvim
```

### Enable True-Color support on (Neo)vim and Tmux w/ terminfo on macOS

Refer to prior-art at from bbqtd's excellent [gist](https://gist.github.com/bbqtd/a4ac060d6f6b9ea6fe3aabe735aa9d95)
and [jdhao's blog](https://jdhao.github.io/2018/10/19/tmux_nvim_true_color/).
There is also extra context on the following tmux issues [here](https://github.com/tmux/tmux/issues/597)
and [here](https://github.com/tmux/tmux/issues/1257).

#### terminfo

Grab the latest terminfo database from
[Thomas E. Dickey](https://invisible-island.net/), who is brilliant and has
worked on projects like
[ncurses](https://invisible-island.net/ncurses/ncurses.html). There is some extra
reading available about [ncurses](https://invisible-island.net/ncurses/ncurses.faq.html),
the
[terminfo database](https://invisible-island.net/ncurses/ncurses.faq.html#terminfo_copying)
and some thoughts on
[copyright notices](https://invisible-island.net/ncurses/ncurses.faq.html#terminfo_copying).
Check out his [GitHub](https://github.com/ThomasDickey)
and [ncurses-snapshots](https://github.com/ThomasDickey/ncurses-snapshots) repo!

1. Download latest terminfo sources

    ```bash
    curl -LO https://raw.githubusercontent.com/ThomasDickey/ncurses-snapshots/master/misc/terminfo.src
    ```

2. Extract terminfo from source to their compiled formats

    The compiled sources will be outputted to your `$HOME/.terminfo` directory

    ```bash
    tic -xe tmux,tmux-256color terminfo.src

    # Feel free to extra other terminfo sources
    tic -xe screen,screen-256color,xterm,xterm-256color,alacritty,alacritty-direct,kitty terminfo.src
    ```

3. Validate the terminfo sources have been "installed"

    ```bash
    infocmp -x tmux-256color
    ```

#### Tmux

With the above terminfo generated and a terminal emulator that supports
"true color" (24-bit), enable the following options in your tmux config.
See the tmux [FAQ](https://github.com/tmux/tmux/wiki/FAQ#how-do-i-use-rgb-colour)
for more details.

```text
set-option -sa terminal-overrides ',*256col*:RGB,'
set-option -ga terminal-overrides ',*256col*:Tc,'
```

#### (Neo)vim

Enable the following setting in your vimrc:

```vim
if (&t_Co == 256 || &t_Co == 88) && has("termguicolors")
    set termguicolors
endif
```

### 'Other' Configs

There are a handful of configs and settings in the
['other'](https://github.com/Happy-Dude/dotfiles/tree/master/other) directory
which is primarily a collection of useful scripts, configs, and preferences I am
currently using or have used in the past.

These are generally not stow-able files since they may not belong in the
user's `$HOME` directory; review them on a case-by-case basis.

## Updating

To update the repo, make sure to pull in the latest changes and update submodules:

NOTE: There may be breaking changes within the repo and submodules. Please use
due diligence and review each project's commit history and changelog before
incorporating updates into your configs.

### Fetching Latest Changes

```bash
cd $HOME/dotfiles
git fetch --all
```

### Applying Latest Changes

```bash
cd $HOME/dotfiles
git pull
git submodule --init --recursive --remote
```

## Misc

### tmux, macOS, and copy-paste

Installing tmux on OS X on using Homebrew (and likely Macports also) would cause
a message saying `launch_msg("SetUserEnvironment"): Socket is not connected`
error. According to a
[StackOverflow](http://stackoverflow.com/questions/10193561/tmux-socket-is-not-connected-error-on-os-x-lion)
answer, and ChrisJohnsen's
[tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard),
it seems that OS X's pbcopy and pbpaste "fail to function properly" for
applicationsthat run on Terminal emulators and have clipboard access (like vim,
tmux, and screen). See thoughtbot's [How to Copy and Paste with tmux on Mac OS X](https://thoughtbot.com/blog/how-to-copy-and-paste-with-tmux-on-mac-os-x)
and [tmux Copy & Paste on OS X: A Better Future](https://thoughtbot.com/blog/tmux-copy-paste-on-os-x-a-better-future)
for context.

Chris Johnsen's patch should fix this problem. Install his
reattach-to-user-namespace wrapper/patch using the following command (if you're
using Homebrew):

```bash
brew install reattach-to-user-namespace
```

If you are using Macports, execute

```bash
port install tmux-pasteboard
```

### Old iptables Notes (circa 2014)

#### iptables Rules

NOTE: This has only worked on Ubuntu-based Linux distributions. I need
to learn more about the boot process in other distributions to make them work
right. I'm looking into seeing how it works for Arch Linux.

```bash
ln -s ~/dotfiles/iptables /etc/iptables; \
ln -s ~/dotfiles/iptables/iptables /etc/init.d/iptables; \
chmod +x /etc/init.d/iptables; \
sudo update-rc.d iptables
```

Execute the script with `service iptables start`. Again, this so far has only
been tested on Ubuntu-based distributions.

To install on Arch Linux (as of April 2013, running systemd), execute the
following commands:

```bash
ln -s ~/dotfiles/iptables/iptables.rules /etc/iptables/iptables.rules
ln -s ~/dotfiles/iptables/ip6tables.rules /etc/ip6tables/ip6tables.rules
sudo systemctl enable iptables.service
```

#### iptables Files

I decided to include my IPTables configuration with this repository, located in
the `other/iptables` directory.

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

## Meta

* Last change: Fri Sep 11 01:22:59 CDT 2020
* Maintainer: Stanley Chan, a.k.a. [Happy-Dude](https://github.com/happy-dude)

## License

The files and code contributed from myself are in the public domain.

You are free to use, transform, adapt, and/or remix my content however you like.

The submodules included in this project repo have their own maintainers and licenses.
Please visit the respective projects' repos to learn more.
