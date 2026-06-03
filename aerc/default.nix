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

  programs.gpg = {
    enable = true;
    settings = {
      use-agent = true;
    };
  };
  home.file.".gnupg/gpg-agent.conf".text = ''
    pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
    allow-loopback-pinentry
  '';

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
        path = "${config.home.homeDirectory}/.mail";
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

    extraAccounts = {
      dev = {
        source = "notmuch://~/.mail/";
        maildir-store = "~/.mail/";
        query-map = "~/.config/aerc/notmuch-map.conf";
        check-mail-cmd = "mbsync lostsanctum && notmuch new";
        check-mail-timeout = "90s";
        default = "Unread";
        outgoing = "smtps://schan%40lostsanctum.dev@smtp.migadu.com:465";
        from = "Stanley Chan <schan@lostsanctum.dev>";
        copy-to = "Sent";
        cache-headers = true;
        outgoing-cred-cmd = "pass show email/aerc/schan@lostsanctum.dev";
        folders-sort = "linux-api,linux-bcachefs,linux-bpf,linux-cgroups,linux-debuggers,linux-io-uring,linux-netdev,linux-next,linux-perf-users,linux-rust,linux-security-module,linux-selinux,linux-toolchains,linux-trace-devel,linux-kernel-announce,INBOX,Unread,Drafts,Sent,Archive,Trash,Junk,ctlug,openbsd-tech,openbsd-announce,freebsd-security,freebsd-announce,dragonfly-commits,dragonfly-users,netbsd-tech-kern,netbsd-announce,ros-diffs,ros-announce,illumos-commits";
      };
    };

    extraConfig = {
      general = {
        unsafe-accounts-conf = true;
        enable-osc8 = true;
      };

      ui = {
        index-columns = "date<12,name<18,flags>2,subject<*";
        column-name = " {{index (.From | names) 0}}";
        column-to = " {{index (.To | names) 0}}";
        column-separator = " ⋮ ";
        timestamp-format = "2006-01-02 03:04 PM MST";
        mouse-enabled = true;
        styleset-name = "gruvbox_material_dark_medium";
        threading-enabled = true;
        show-thread-context = true;
      };

      viewer = {
        pager = ''bat -p --pager="less --mouse --RAW-CONTROL-CHARS --quit-if-one-screen --hilite-search --ignore-case --LONG-PROMPT --chop-long-lines --CLEAR-SCREEN"'';
      };

      compose = {
        empty-subject-warning = true;
        no-attachment-warning = "^[^>]*attach(ed|ment)";
      };

      # Only filters needs raw string format due to duplicate text/html entries
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
    };

    extraBinds = {
      global = {
        # Binds are of the form <key sequence> = <command to run>
        # To use '=' in a key sequence, substitute it with "Eq": "<Ctrl+Eq>"
        # If you wish to bind #, you can wrap the key sequence in quotes: "#" = quit
        "<C-p>" = ":prev-tab<Enter>";
        "<C-PgUp>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
        "<C-PgDn>" = ":next-tab<Enter>";
        "<C-t>" = ":term<Enter>";
        "?" = ":help keys<Enter>";
        "<C-c>" = ":prompt 'Quit?' quit<Enter>";
        "<C-q>" = ":prompt 'Quit?' quit<Enter>";
        "<C-z>" = ":suspend<Enter>";
      };

      messages = {
        q = ":prompt 'Quit?' quit<Enter>";

        "<C-Down>" = ":send-keys \\<PgDn\\><Enter>";
        "<C-Up>" = ":send-keys \\<PgUp\\><Enter>";

        # Navigation
        j = ":next<Enter>";
        "<Down>" = ":next<Enter>";
        "<C-d>" = ":next 50%<Enter>";
        "<C-f>" = ":next 100%<Enter>";
        "<PgDn>" = ":next 100%<Enter>";

        k = ":prev<Enter>";
        "<Up>" = ":prev<Enter>";
        "<C-u>" = ":prev 50%<Enter>";
        "<C-b>" = ":prev 100%<Enter>";
        "<PgUp>" = ":prev 100%<Enter>";
        g = ":select 0<Enter>";
        G = ":select -1<Enter>";

        # Folder navigation
        J = ":next-folder<Enter>";
        #"<C-Down>" = ":next-folder<Enter>";
        K = ":prev-folder<Enter>";
        #"<C-Up>" = ":prev-folder<Enter>";
        H = ":collapse-folder<Enter>";
        "<C-Left>" = ":collapse-folder<Enter>";
        L = ":expand-folder<Enter>";
        "<C-Right>" = ":expand-folder<Enter>";

        # Marking
        v = ":mark -t<Enter>";
        "<Space>" = ":mark -t<Enter>:next<Enter>";
        V = ":mark -v<Enter>";

        # Threading
        T = ":toggle-threads<Enter>";
        zc = ":fold<Enter>";
        zo = ":unfold<Enter>";

        # Actions
        "<Enter>" = ":view<Enter>";
        d = ":prompt 'Really delete this message?' 'delete-message'<Enter>";
        D = ":delete<Enter>";
        a = ":archive flat<Enter>";
        A = ":unmark -a<Enter>:mark -T<Enter>:archive flat<Enter>";

        # Compose
        C = ":compose<Enter>";
        m = ":compose<Enter>";

        # Reply
        rr = ":reply -a<Enter>";
        rq = ":reply -aq<Enter>";
        Rr = ":reply<Enter>";
        Rq = ":reply -q<Enter>";

        # Commands
        c = ":cf<space>";
        "$" = ":term<space>";
        "!" = ":term<space>";
        "|" = ":pipe<space>";

        # Git integration
        ga = ":flag<Enter>:pipe -mb git am -3<Enter>";
        gp = ":term git push<Enter>";
        gl = ":term git log<Enter>";

        # Search and filter
        "/" = ":search<space>";
        "\\" = ":filter<space>";
        n = ":next-result<Enter>";
        N = ":prev-result<Enter>";
        "<Esc>" = ":clear<Enter>";

        # Split
        s = ":split<Enter>";
        S = ":vsplit<Enter>";
      };

      "messages:folder=Drafts" = {
        "<Enter>" = ":recall<Enter>";
      };

      view = {
        "/" = ":toggle-key-passthrough<Enter>/";
        q = ":close<Enter>";
        O = ":open<Enter>";
        o = ":open<Enter>";
        S = ":save<space>";
        "|" = ":pipe<space>";
        D = ":delete<Enter>";
        A = ":archive flat<Enter>";

        "<C-l>" = ":open-link <space>";

        # Forward and reply
        f = ":forward -A<Enter>";
        F = ":forward -F<Enter>";
        rr = ":reply -a<Enter>";
        rq = ":reply -aq<Enter>";
        rt = ":reply -Tthanks<Enter>";
        Rr = ":reply<Enter>";
        Rq = ":reply -q<Enter>";

        # Git integration
        ga = ":pipe -b git am -3<Enter>";
        gp = ":term git push<Enter>";
        gl = ":term git log<Enter>";

        # Navigation
        H = ":toggle-headers<Enter>";
        "<C-k>" = ":prev-part<Enter>";
        "<C-Up>" = ":prev-part<Enter>";
        "<C-j>" = ":next-part<Enter>";
        "<C-Down>" = ":next-part<Enter>";
        J = ":next<Enter>";
        "<C-Right>" = ":next<Enter>";
        K = ":prev<Enter>";
        "<C-Left>" = ":prev<Enter>";
      };

      "view::passthrough" = {
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<Esc>" = ":toggle-key-passthrough<Enter>";
      };

      compose = {
        # Keybindings used when the embedded terminal is not selected in the compose view
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<C-k>" = ":prev-field<Enter>";
        "<C-Up>" = ":prev-field<Enter>";
        "<C-j>" = ":next-field<Enter>";
        "<C-Down>" = ":next-field<Enter>";
        "<A-p>" = ":switch-account -p<Enter>";
        "<C-Left>" = ":switch-account -p<Enter>";
        "<A-n>" = ":switch-account -n<Enter>";
        "<C-Right>" = ":switch-account -n<Enter>";
        "<tab>" = ":next-field<Enter>";
        "<backtab>" = ":prev-field<Enter>";
        "<C-p>" = ":prev-tab<Enter>";
        "<C-PgUp>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
        "<C-PgDn>" = ":next-tab<Enter>";
      };

      "compose::editor" = {
        # Keybindings used when the embedded terminal is selected in the compose view
        "$noinherit" = "true";
        "$ex" = "<C-x>";
        "<C-k>" = ":prev-field<Enter>";
        "<C-Up>" = ":prev-field<Enter>";
        "<C-j>" = ":next-field<Enter>";
        "<C-Down>" = ":next-field<Enter>";
        "<C-p>" = ":prev-tab<Enter>";
        "<C-PgUp>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
        "<C-PgDn>" = ":next-tab<Enter>";
      };

      "compose::review" = {
        # Keybindings used when reviewing a message to be sent
        y = ":send<Enter>";
        n = ":abort<Enter>";
        v = ":preview<Enter>";
        p = ":postpone<Enter>";
        q = ":choose -o d discard abort -o p postpone postpone<Enter>";
        e = ":edit<Enter>";
        a = ":attach<space>";
        d = ":detach<space>";
        c = ":encrypt<Enter>";
        s = ":sign<Enter>";
        V = ":header -f X-Sourcehut-Patchset-Update NEEDS_REVISION<Enter>";
        A = ":header -f X-Sourcehut-Patchset-Update APPLIED<Enter>";
        R = ":header -f X-Sourcehut-Patchset-Update REJECTED<Enter>";
      };

      terminal = {
        "$noinherit" = "true";
        "$ex" = "<C-x>";

        "<C-p>" = ":prev-tab<Enter>";
        "<C-n>" = ":next-tab<Enter>";
        "<C-PgUp>" = ":prev-tab<Enter>";
        "<C-PgDn>" = ":next-tab<Enter>";
      };
    };

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
        Thanks!
        ---
        Stan

        {{exec "{ git remote get-url --push origin; git reflog -2 origin/master --pretty=format:%h | xargs printf '%s\n' | tac; } | xargs printf 'To %s\n   %s..%s  master -> master'" ""}}
      '';

      quoted_thanks = ''
        Thanks!
        ---
        Stan

        {{exec "{ git remote get-url --push origin; git reflog -2 origin/master --pretty=format:%h | xargs printf '%s\n' | tac; } | xargs printf 'To %s\n   %s..%s  master -> master'" ""}}

        On {{dateFormat (.OriginalDate | toLocal) "Mon Jan 2, 2006 at 3:04 PM MST"}}, {{(index .OriginalFrom 0).Name}} wrote:
        {{wrapText .OriginalText 72 | quote}}
      '';
    };
  };
}
