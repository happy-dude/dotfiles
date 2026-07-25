{pkgs, ...}: {
  programs.gpg = {
    enable = true;
    settings = {
      use-agent = true;
    };
  };

  home.file.".gnupg/gpg-agent.conf".text = ''
    pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
    allow-loopback-pinentry
  '';
}
