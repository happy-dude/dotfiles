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
if GIT_PATH="$(command -v git 2>/dev/null)"; then
  echo "Git found: cloning dotfiles repository into $HOME/dotfiles"
  "$GIT_PATH" clone --recursive \
    https://github.com/Happy-Dude/dotfiles.git \
    "$HOME/dotfiles"
  "$GIT_PATH" -C "$HOME/dotfiles" submodule update --init --recursive
  "$GIT_PATH" -C "$HOME/dotfiles" submodule foreach 'git checkout master'
else
  echo "Git not found; please install git or download the zip archive of repo"
fi

if command -v vim >/dev/null 2>&1; then
  echo "vim found"

  if [ -f "$HOME/.vimrc" ]; then
    echo "Backing up existing .vimrc file to .vimrc.bak"
    mv "$HOME/.vimrc" "$HOME/.vimrc.bak"
  fi

  if [ -d "$HOME/.cache" ]; then
    if [ -d "$HOME/.cache/vim" ]; then
      echo "Backing up existing vim cache directory in .cache"
      mv "$HOME/.cache/vim" "$HOME/.cache/vim.bak"
    fi
  fi

  echo "Creating vim backup, cache, undo, and view directories in $HOME/.cache/vim"
  mkdir -p \
    "$HOME/.cache/vim" \
    "$HOME/.cache/vim/backup" \
    "$HOME/.cache/vim/swap" \
    "$HOME/.cache/vim/undo" \
    "$HOME/.cache/vim/view"

  echo "Linking $HOME/dotfiles/vim to $HOME/.vim"
  ln -s "$HOME/dotfiles/vim" "$HOME/.vim"

  echo "Linking $HOME/dotfiles/vim/vimrc to $HOME/.vimrc"
  ln -s "$HOME/dotfiles/vim/vimrc" "$HOME/.vimrc"
fi

if command -v nvim >/dev/null 2>&1; then
  echo "nvim found"

  if [ -f "$HOME/.nvimrc" ]; then
    echo "Backing up existing .nvimrc file to .nvimrc.bak"
    mv "$HOME/.nvimrc" "$HOME/.nvimrc.bak"
  fi

  echo "Linking $HOME/dotfiles/vim to $HOME/.nvim"
  ln -s "$HOME/dotfiles/vim" "$HOME/.nvim"

  echo "Linking $HOME/dotfiles/vim/vimrc to $HOME/.nvimrc"
  ln -s "$HOME/dotfiles/vim/vimrc" "$HOME/.nvimrc"
fi

if command -v tmux >/dev/null 2>&1; then
  echo "tmux found"

  if [ -f "$HOME/.tmux.conf" ]; then
    echo "Backing up existing .tmux.conf file to .tmux.conf.bak"
    mv "$HOME/.tmux.conf" "$HOME/.tmux.conf.bak"
  fi

  echo "Linking $HOME/dotfiles/tmux/tmux.conf to $HOME/.tmux.conf"
  ln -s "$HOME/dotfiles/tmux/tmux.conf" "$HOME/.tmux.conf"
fi

if command -v zsh >/dev/null 2>&1; then
  echo "zsh found"

  if [ -f "$HOME/.zshrc" ]; then
    echo "Backing up existing .zshrc file to .zshrc.bak"
    mv "$HOME/.zshrc" "$HOME/.zshrc.bak"
  fi

  echo "Linking Sorin's zsh-prezto"
  ln -s "$HOME/dotfiles/zsh/prezto-sorin" "$HOME/.zprezto"
  for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/*; do
    base="$(basename "$rcfile")"
    [ "$base" = "README.md" ] && continue
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${base}"
  done

  echo "Changing default shell to zsh"
  chsh -s "$(command -v zsh)"
fi

if command -v iptables >/dev/null 2>&1; then
  echo "iptables found"

  if [ -f "/etc/iptables/iptables.rules" ]; then
    echo "Backing up existing IPTables ruleset"
    mv /etc/iptables/iptables.rules /etc/iptables/iptables.rules.bak
  fi

  echo "Linking $HOME/dotfiles/iptables/iptables.rules to /etc/iptables/iptables.rules"
  ln -s "$HOME/dotfiles/iptables/iptables.rules" /etc/iptables/iptables.rules

  echo "Enabling iptables systemd unit"
  systemctl enable iptables.service
fi

if command -v ip6tables >/dev/null 2>&1; then
  echo "IP6Tables found"

  if [ -f "/etc/iptables/ip6tables.rules" ]; then
    echo "Backing up existing IP6Tables ruleset"
    mv /etc/iptables/ip6tables.rules /etc/iptables/ip6tables.rules.bak
  fi

  echo "Linking $HOME/dotfiles/iptables/ip6tables.rules to /etc/ip6tables/iptables.rules"
  ln -s "$HOME/dotfiles/iptables/ip6tables.rules" /etc/ip6tables/iptables.rules

  echo "Enabling ip6tables systemd unit"
  systemctl enable ip6tables.service
fi

if command -v slim >/dev/null 2>&1; then
  echo "slim found"

  if [ -f "/etc/slim.conf" ]; then
    echo "Backing up existing slim configuration"
    mv /etc/slim.conf /etc/slim.conf.bak
  fi

  echo "Linking $HOME/dotfiles/slim/slim.conf to /etc/slim/slim.conf"
  ln -s "$HOME/dotfiles/slim/slim.conf" /etc/slim/slim.conf

  echo "Enabling slim systemd unit"
  systemctl enable slim.service
fi
