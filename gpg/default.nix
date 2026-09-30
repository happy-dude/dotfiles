{pkgs, ...}: {
  programs.gpg.enable = true;

  # GnuPG 2.1 and later always use the agent and allow loopback pinentry by
  # default, so only the pinentry program needs stating. The agent also
  # prompts for callers without a terminal, such as a GPG-backed KWallet, so
  # use the Qt flavour from the installed pinentry-all. Without a display,
  # pass --pinentry-mode loopback to gpg.
  home.file.".gnupg/gpg-agent.conf".text = ''
    pinentry-program ${pkgs.pinentry-all}/bin/pinentry-qt
  '';
}
