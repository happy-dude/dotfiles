{pkgs, ...}: {
  programs.gpg.enable = true;

  # GnuPG 2.1 and later always use the agent and allow loopback pinentry by
  # default, so only the pinentry program needs stating. pinentry-all is the
  # installed package; take the curses flavour from it.
  home.file.".gnupg/gpg-agent.conf".text = ''
    pinentry-program ${pkgs.pinentry-all}/bin/pinentry-curses
  '';
}
