{
  config,
  lib,
  ...
}: let
  lessFlags = import ../lib/less-flags.nix;
  queryMap = "aerc/notmuch-map.conf";
in {
  xdg.configFile.${queryMap}.source = ./.config/aerc/notmuch-map.conf;

  programs.aerc = {
    enable = true;

    # aerc expands ~ in query-map, not XDG variables. Use the same path
    # Home Manager deploys even when configHome is not ~/.config.
    extraAccounts =
      builtins.replaceStrings ["~/.config/${queryMap}"]
      ["${config.xdg.configHome}/${queryMap}"]
      (builtins.readFile ./.config/aerc/accounts.conf);

    # aerc.conf stays an attribute set: Home Manager reads
    # general.unsafe-accounts-conf from it to decide whether aerc will start
    # against a store-resident accounts.conf, and cannot see that setting
    # inside a string. The tracked aerc.conf mirrors this, and the
    # aerc-config-mirror check fails if the two ever disagree.
    extraConfig = {
      general.unsafe-accounts-conf = true;

      ui = {
        index-columns = "date<12,name<18,flags>2,subject<*";
        column-name = "{{index (.From | names) 0}}";
        column-to = "{{index (.To | names) 0}}";
        column-separator = "⋮";
        timestamp-format = "2006-01-02 03:04 PM MST";
        mouse-enabled = true;
        styleset-name = "gruvbox_material_dark_medium";
        threading-enabled = true;
        show-thread-context = true;
      };

      viewer = {
        pager = ''bat -p --pager="less ${lessFlags}"'';
      };

      compose = {
        empty-subject-warning = true;
        no-attachment-warning = "^[^>]*attach(ed|ment)";
      };

      # aerc uses the first matching MIME glob. Keep specific filters ahead
      # of text/* rather than allowing an attribute set to sort the keys.
      filters = ''
        text/plain=colorize
        text/calendar=calendar
        message/delivery-status=colorize
        message/rfc822=colorize
        text/html=pandoc --sandbox -f html -t plain | colorize
        text/*=bat -fP --file-name="$AERC_FILENAME"
        .headers=colorize
      '';
    };

    # aerc's own binds.conf is the source of truth; Home Manager reads it
    # verbatim so the file stays editable in aerc's native format.
    extraBinds = builtins.readFile ./.config/aerc/binds.conf;

    stylesets = lib.genAttrs [
      "gruvbox"
      "gruvbox_material_dark_hard"
      "gruvbox_material_dark_medium"
      "gruvbox_material_dark_soft"
    ] (name: builtins.readFile (./.config/aerc/stylesets + "/${name}"));

    # In a headerless template the initial blank line separates the empty
    # headers from the body; do not trim it.
    templates =
      lib.genAttrs ["thanks" "quoted_thanks"] (name:
        builtins.readFile (./.config/aerc/templates + "/${name}"));
  };
}
