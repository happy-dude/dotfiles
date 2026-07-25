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

    # accounts.conf is read from aerc's own file, which is its only copy.
    extraAccounts = builtins.readFile ./.config/aerc/accounts.conf;

    # aerc.conf stays an attribute set: Home Manager reads
    # general.unsafe-accounts-conf from it to decide whether aerc will start
    # against a store-resident accounts.conf, and cannot see that setting
    # inside a string. The tracked aerc.conf mirrors this, and the
    # aerc-config-mirror check fails if the two ever disagree.
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
