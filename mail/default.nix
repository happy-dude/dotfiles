{
  config,
  pkgs,
  ...
}: {
  programs.mbsync = {
    enable = true;
    extraConfig =
      builtins.replaceStrings
      ["/etc/ssl/certs/ca-certificates.crt"]
      ["${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"]
      (builtins.readFile ./.mbsyncrc);
  };

  programs.notmuch = {
    enable = true;
    settings = {
      database.path = "${config.home.homeDirectory}/.mail";
      user = {
        name = "Stanley Chan";
        primary_email = "schan@lostsanctum.dev";
      };
      new.tags = [
        "unread"
        "inbox"
      ];
      search.exclude_tags = [
        "deleted"
        "spam"
      ];
      maildir.synchronize_flags = true;
      crypto.gpg_path = "gpg";
    };
  };
}
