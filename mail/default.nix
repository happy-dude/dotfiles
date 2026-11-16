{
  config,
  lib,
  pkgs,
  ...
}: let
  # The tracked .mbsyncrc names the Debian path of the host CA bundle. Every
  # profile verifies against the Nix bundle instead, so the result does not
  # depend on where a host keeps its own.
  hostCaBundle = "/etc/ssl/certs/ca-certificates.crt";
  mbsyncrc = builtins.readFile ./.mbsyncrc;
in {
  programs.mbsync = {
    enable = true;
    # replaceStrings returns its input unchanged when the path is absent,
    # which would deploy whatever CertificateFile .mbsyncrc names by then.
    extraConfig =
      lib.throwIfNot (lib.hasInfix hostCaBundle mbsyncrc)
      "mail/.mbsyncrc no longer names ${hostCaBundle}; update hostCaBundle in mail/default.nix to the CertificateFile path it should replace"
      (
        builtins.replaceStrings
        [hostCaBundle]
        ["${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"]
        mbsyncrc
      );
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
