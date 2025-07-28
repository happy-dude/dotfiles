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

    extraConfig = {
      general = {
        "unsafe-accounts-conf" = true;
        "enable-osc8" = true;
      };

      ui = ''
        index-columns=date<12,name<18,flags>2,subject<*
        column-name=" {{index (.From | names) 0}}"
        column-to=" {{index (.To | names) 0}}"
        column-separator=" ⋮ "
        timestamp-format=2006-01-02 03:04 PM MST
        mouse-enabled=true
        styleset-name=gruvbox_material_dark_medium
        threading-enabled=true
        show-thread-context=true
      '';

      statusline = "";

      viewer = ''
        pager=bat -p --pager="less --mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --window=-4 --CLEAR-SCREEN"
      '';

      compose = ''
        empty-subject-warning=true
        no-attachment-warning=^[^>]*attach(ed|ment)
      '';

      multipart-converters = "";

      filters = ''
        text/plain=colorize
        text/calendar=calendar
        message/delivery-status=colorize
        message/rfc822=colorize
        text/html=pandoc -f html -t plain | colorize
        text/html=html | colorize
        text/*=bat -fP --file-name="$AERC_FILENAME"
        .headers=colorize
      '';

      openers = "";

      hooks = "";

      templates = "";
    };

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
