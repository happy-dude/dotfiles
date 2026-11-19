{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  checkHome = profile: home: let
    cfg = home.config;
    bootstrap =
      lib.findFirst (pkg: lib.getName pkg == "mail-bootstrap")
      (throw "${profile} does not install mail-bootstrap")
      cfg.home.packages;
    notmuch = lib.getExe cfg.programs.notmuch.package;
    configFile = cfg.xdg.configFile."notmuch/default/config".source;
    isyncrc = cfg.xdg.configFile."isyncrc".source;
  in ''
    export HOME="$TMPDIR/${profile}"
    export XDG_CONFIG_HOME="$HOME/xdg"
    export XDG_DATA_HOME="$HOME/data"
    hooks="$XDG_CONFIG_HOME/notmuch/default/hooks"
    mkdir -p "$hooks"
    ln -s ${configFile} "$hooks/../config"
    cat > "$hooks/pre-new" <<'HOOK'
    #!${pkgs.runtimeShell}
    touch "$HOME/hook-ran"
    exit 1
    HOOK
    chmod +x "$hooks/pre-new"
    ln -s pre-new "$hooks/post-new"

    # A shell left over from another profile must not redirect the bootstrap.
    NOTMUCH_CONFIG="$HOME/wrong-config" NOTMUCH_DATABASE="$HOME/wrong-db" \
      NOTMUCH_PROFILE=other ${lib.getExe bootstrap}
    test ! -e "$HOME/wrong-db"
    test ! -e "$HOME/hook-ran"
    test "$(stat -c %a "$HOME/.mail")" = 700
    test -d "$HOME/.mail/.notmuch/xapian"
    test "$(${notmuch} count '*')" = 0

    # Re-running bootstrap must not index even a newly arrived message.
    mkdir -p "$HOME/.mail/INBOX/"{cur,new,tmp}
    cat > "$HOME/.mail/INBOX/new/fixture" <<'MESSAGE'
    From: Sender <sender@example.org>
    To: Recipient <recipient@example.org>
    Subject: Existing mail
    Message-ID: <bootstrap@example.org>
    Date: Thu, 01 Jan 1970 00:00:00 +0000

    Keep this message intact.
    MESSAGE
    ${lib.getExe bootstrap}
    test "$(${notmuch} count '*')" = 0
    ${notmuch} new --no-hooks
    test "$(${notmuch} count 'id:bootstrap@example.org')" = 1
    test ! -e "$HOME/hook-ran"
    # Positive control: the deployed XDG config does discover the fixture
    # hook when an ordinary user-initiated scan allows hooks.
    if ${notmuch} new || [[ ! -f $HOME/hook-ran ]]; then
      echo 'notmuch did not run the refusing test hook' >&2
      exit 1
    fi

    # The selected store is a Maildir fixture, never the IMAP store or the
    # channel. --list or --dry-run on the *channel* would still contact IMAP.
    grep -Fx 'CertificateFile ${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt' ${isyncrc}
    grep -Fx 'MaildirStore lostsanctum-local' ${isyncrc}
    ${lib.getExe' cfg.programs.mbsync.package "mbsync"} -c ${isyncrc} \
      --list-stores lostsanctum-local > "$HOME/local-folders"
    grep -Fx INBOX "$HOME/local-folders"

    export HOME="$TMPDIR/${profile}-occupied"
    mkdir -p "$HOME/.mail"
    echo existing > "$HOME/.mail/.marker"
    if ${lib.getExe bootstrap}; then
      echo 'accepted a nonempty unindexed mail root' >&2
      exit 1
    fi
    test "$(cat "$HOME/.mail/.marker")" = existing
    test ! -e "$HOME/.mail/.notmuch"
    mkdir "$HOME/.mail/.notmuch"
    echo existing-index-data > "$HOME/.mail/.notmuch/marker"
    if ${lib.getExe bootstrap} 2> refusal; then
      echo 'accepted a broken notmuch index' >&2
      exit 1
    fi
    grep -F 'Cannot read the existing index' refusal
    test "$(cat "$HOME/.mail/.notmuch/marker")" = existing-index-data
    mv "$HOME/.mail" "$HOME/original"
    mkdir "$HOME/elsewhere"
    ln -s elsewhere "$HOME/.mail"
    if ${lib.getExe bootstrap}; then
      echo 'accepted a symlinked mail root' >&2
      exit 1
    fi
    test ! -e "$HOME/elsewhere/.notmuch"
  '';
in {
  mail-bootstrap = mkCheck {
    name = "mail-bootstrap-check";
    script = assert homes != {}; lib.concatStringsSep "\n" (lib.mapAttrsToList checkHome homes);
  };
}
