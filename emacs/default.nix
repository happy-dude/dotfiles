{ config, pkgs, ... }:

{
  home.file.".emacs".source = ./init.el;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
    extraPackages = epkgs: with epkgs; [
      annalist
      compat
      dash
      editorconfig
      emacsql
      emacsql-sqlite
      evil
      evil-collection
      evil-org
      evil-rsi
      evil-surround
      f
      magit
      markdown-mode
      nix-mode
      noflet
      notmuch
      org
      org-journal
      org-roam
      org-roam-ui
      queue
      rainbow-delimiters
      s
      slime
      solarized-theme
      transient
      undo-tree
      use-package
      web-server
      websocket
      which-key
      with-editor
      yaml
      yaml-mode
    ];
  };
}
