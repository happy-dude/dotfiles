{...}: {
  #xdg.configFile."aerc/aerc.conf".source = ./.config/aerc/aerc.conf;
  #xdg.configFile."aerc/accounts.conf".source = ./.config/aerc/accounts.conf;
  xdg.configFile."aerc/notmuch-map.conf".source = ./.config/aerc/notmuch-map.conf;

  #xdg.configFile."aerc/templates/quoted_thanks".source = ./.config/aerc/templates/quoted_thanks;
  #xdg.configFile."aerc/templates/thanks".source = ./.config/aerc/templates/thanks;

  #xdg.configFile."aerc/stylesets/gruvbox".source = ./.config/aerc/stylesets/gruvbox;
  #xdg.configFile."aerc/stylesets/gruvbox_material_dark_hard".source = ./.config/aerc/stylesets/gruvbox_material_dark_hard;
  #xdg.configFile."aerc/stylesets/gruvbox_material_dark_medium".source = ./.config/aerc/stylesets/gruvbox_material_dark_medium;
  #xdg.configFile."aerc/stylesets/gruvbox_material_dark_soft".source = ./.config/aerc/stylesets/gruvbox_material_dark_soft;

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

    # aerc's own binds.conf is the source of truth; Home Manager reads it
    # verbatim so the file stays editable in aerc's native format.
    extraBinds = builtins.readFile ./.config/aerc/binds.conf;

    stylesets = {
      gruvbox = ''
        ${builtins.readFile ./.config/aerc/stylesets/gruvbox}
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
