{config, ...}: {
  #home.file.".mbsyncrc".source = ./.mbsyncrc;
  #xdg.configFile."notmuch/default/config".source = ./.notmuch-config;

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
      Expunge Both
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
        tags = "unread;inbox;";
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
}
