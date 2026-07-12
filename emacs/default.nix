{
  config,
  pkgs,
  ...
}: {
  home.file = {
    ".emacs".source = ./init.el;
    "org/.dir-locals.el".source = ./org-dir-locals.el;
  };

  home.activation = {
    createOrgDirectories = config.lib.dag.entryAfter ["writeBoundary"] ''
      ${pkgs.coreutils}/bin/mkdir -p \
        "$HOME/org/Archive" \
        "$HOME/org/roam" \
        "${config.xdg.cacheHome}/emacs"
    '';

    registerOrgProtocol = config.lib.dag.entryAfter ["linkGeneration"] ''
      $DRY_RUN_CMD ${pkgs.xdg-utils}/bin/xdg-mime \
        default emacsclient.desktop x-scheme-handler/org-protocol
    '';
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
    extraPackages = epkgs:
      with epkgs; [
        annalist
        compat
        dash
        editorconfig
        emacsql
        evil
        evil-collection
        evil-org
        evil-rsi
        evil-surround
        f
        magit
        markdown-mode
        nix-mode
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

  services.emacs = {
    enable = true;
    startWithUserSession = "graphical";
  };
}
