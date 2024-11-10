{ config, pkgs, ... }:

{
  #home.file.".mbsyncrc".source = ./.mbsyncrc;
  #xdg.configFile."notmuch/default/config".source = ./.notmuch-config;

  #xdg.configFile."aerc/aerc.conf".source = ./.config/aerc/aerc.conf;
  #xdg.configFile."aerc/accounts.conf".source = ./.config/aerc/accounts.conf;
  #xdg.configFile."aerc/binds.conf".source = ./.config/aerc/binds.conf;
  xdg.configFile."aerc/notmuch-map.conf".source = ./.config/aerc/notmuch-map.conf;

  #xdg.configFile."aerc/templates/quoted_thanks".source = ./.config/aerc/templates/quoted_thanks;
  #xdg.configFile."aerc/templates/thanks".source = ./.config/aerc/templates/thanks;

  #xdg.configFile."aerc/stylesets/gruvbox".source = ./.config/aerc/stylesets/gruvbox;
  #xdg.configFile."aerc/stylesets/gruvbox_material_dark_hard".source = ./.config/aerc/stylesets/gruvbox_material_dark_hard;
  #xdg.configFile."aerc/stylesets/gruvbox_material_dark_medium".source = ./.config/aerc/stylesets/gruvbox_material_dark_medium;
  #xdg.configFile."aerc/stylesets/gruvbox_material_dark_soft".source = ./.config/aerc/stylesets/gruvbox_material_dark_soft;

  programs.mbsync = {
    enable = true;
    extraConfig = ''
      IMAPAccount lostsanctum
      Host imap.migadu.com
      User schan@lostsanctum.dev
      PassCmd "pass show email/aerc/schan@lostsanctum.dev"
      TLSType IMAPS
      CertificateFile /etc/ssl/certs/ca-certificates.crt

      IMAPStore lostsanctum-remote
      Account lostsanctum

      MaildirStore lostsanctum-local
      Path ~/.mail/
      Inbox ~/.mail/INBOX
      SubFolders Verbatim

      Channel lostsanctum
      Far :lostsanctum-remote:
      Near :lostsanctum-local:
      Patterns *
      Create Both
      SyncState *
    '';
  };

  programs.notmuch = {
    enable = true;
    extraConfig = {
      database = {
        path = "/home/stanleychan/.mail";
      };
      user = {
        name = "Stanley Chan";
        primary_email = "schan@lostsanctum.dev";
      };
      new = {
        tags = "unread;inbox;sent;";
        ignore = "";
      };
      search = {
        exclude_tags = "deleted;spam;";
      };
      maildir = {
        synchronize_flags = "true";
      };
      crypto = {
        gpg_path = "gpg";
      };
    };
  };

  programs.aerc = {
    enable = true;

    extraAccounts =
      let
        iniFormat = pkgs.formats.iniWithGlobalSection { };
        cfgText = iniFormat.generate "accounts.conf" {
          globalSection = { };
          sections = { };
        };
      in
      ''
        ${builtins.readFile .config/aerc/accounts.conf}
      '';

    extraConfig =
      let
        iniFormat = pkgs.formats.iniWithGlobalSection { };
        cfgText = iniFormat.generate "aerc.conf" {
          globalSection = { };
          sections = { };
        };
      in
      ''
        ${builtins.readFile .config/aerc/aerc.conf}
      '';

    extraBinds =
      let
        iniFormat = pkgs.formats.iniWithGlobalSection { };
        cfgText = iniFormat.generate "binds.conf" {
          globalSection = { };
          sections = { };
        };
      in
      ''
        ${builtins.readFile .config/aerc/binds.conf}
      '';

    stylesets = {
      gruvbox = ''
        ${builtins.readFile .config/aerc/stylesets/gruvbox}
      '';
      gruvbox_material_dark_hard = ''
        ${builtins.readFile ./.config/aerc/stylesets/gruvbox_material_dark_hard}
      '';
      gruvbox_material_dark_medium = ''
        ${builtins.readFile ./.config/aerc/stylesets/gruvbox_material_dark_medium}
      '';
      gruvbox_material_dark_soft = ''
        ${builtins.readFile ./.config/aerc/stylesets/gruvbox_material_dark_soft}
      '';
    };

    templates = {
      thanks = ''
        ${builtins.readFile ./.config/aerc/templates/thanks}
      '';
      quoted_thanks = ''
        ${builtins.readFile ./.config/aerc/templates/quoted_thanks}
      '';
    };
  };
}
