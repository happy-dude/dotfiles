{
  config,
  lib,
  pkgs,
  ...
}: let
  # The tracked .mbsyncrc names the Debian path of the host CA bundle. Every
  # profile verifies against the Nix bundle instead, so the result does not
  # depend on where a host keeps its own.
  hostCertificate = "CertificateFile /etc/ssl/certs/ca-certificates.crt";
  lines = lib.splitString "\n" (builtins.readFile ./.mbsyncrc);
  certificates = builtins.filter (line:
    builtins.match "[[:space:]]*certificatefile([[:space:]].*)?" (lib.toLower line) != null)
  lines;
  # Non-absolute database paths are relative to HOME in notmuch-config(1).
  mailRoot = ".mail";
  bootstrap = pkgs.writeShellApplication {
    name = "mail-bootstrap";
    runtimeInputs = [pkgs.coreutils pkgs.findutils config.programs.notmuch.package];
    text = ''
      umask 077
      # Use exactly the config backing this profile's XDG link. A different
      # NOTMUCH_DATABASE takes precedence even over an explicit config file.
      unset NOTMUCH_DATABASE NOTMUCH_PROFILE
      export NOTMUCH_CONFIG=${config.xdg.configFile."notmuch/default/config".source}
      root="$HOME/${mailRoot}"
      if [[ -L $root ]]; then
        echo "Refusing to initialize a symlinked mail root: $root" >&2
        exit 1
      fi
      mkdir -p -- "$root"
      entry=$(find "$root" -mindepth 1 -maxdepth 1 -printf . -quit)
      if [[ -n $entry ]]; then
        if notmuch count '*' >/dev/null; then
          echo "The local index is already initialized."
          exit 0
        fi
        if [[ -e $root/.notmuch || -L $root/.notmuch ]]; then
          echo "Cannot read the existing index at $root/.notmuch; leaving it untouched." >&2
        else
          echo "Refusing to initialize a nonempty mail root without an index: $root" >&2
        fi
        exit 1
      fi
      exec notmuch new --no-hooks
    '';
  };
in {
  # Mail is optional. Switching either profile installs the command, but
  # never creates an index, imports mail, or tries to obtain credentials.
  home.packages = [bootstrap];

  programs.mbsync = {
    enable = true;
    extraConfig =
      lib.throwIfNot (certificates == [hostCertificate])
      "mail/.mbsyncrc must have exactly one active '${hostCertificate}' directive; update the CA substitution in mail/default.nix"
      (lib.concatMapStringsSep "\n" (line:
        if line == hostCertificate
        then "CertificateFile ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
        else line)
      lines);
  };

  programs.notmuch = {
    enable = true;
    settings = {
      database.path = mailRoot;
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
