{
  config,
  pkgs,
  ...
}: {
  home.file."org/.dir-locals.el".source = ./org-dir-locals.el;

  xdg.configFile."emacs/init.el".source = ./init.el;

  home.activation = {
    createOrgDirectories = config.lib.dag.entryAfter ["writeBoundary"] ''
      ${pkgs.coreutils}/bin/mkdir -p \
        "$HOME/org/Archive" \
        "$HOME/org/roam" \
        "${config.xdg.cacheHome}/emacs/undo-tree"
    '';

    registerOrgProtocol = config.lib.dag.entryAfter ["linkGeneration"] ''
      $DRY_RUN_CMD ${pkgs.xdg-utils}/bin/xdg-mime \
        default emacs-org-protocol.desktop x-scheme-handler/org-protocol
    '';
  };

  xdg.desktopEntries.emacs-org-protocol = {
    name = "Emacs Org Protocol";
    exec = "${pkgs.emacs-pgtk}/bin/emacsclient --alternate-editor= --create-frame --no-wait %u";
    icon = "emacs";
    mimeType = ["x-scheme-handler/org-protocol"];
    noDisplay = true;
    terminal = false;
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
