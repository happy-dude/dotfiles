{
  config,
  pkgs,
  ...
}: {
  xdg.enable = true;
  xdg.mime.enable = true;
  targets.genericLinux.enable = true;

  xdg.desktopEntries.ghostty = {
    name = "Ghostty (nix)";
    type = "Application";
    comment = "A terminal emulator";
    icon = "com.mitchellh.ghostty";
    exec = "env LIBGL_ALWAYS_SOFTWARE=true ${config.lib.nixGL.wrap pkgs.ghostty}/bin/ghostty";
    categories = [
      "System"
      "TerminalEmulator"
    ];
    startupNotify = true;
    terminal = false;
    actions = {
      new-window = {
        name = "New Window";
        exec = "env LIBGL_ALWAYS_SOFTWARE=true ${config.lib.nixGL.wrap pkgs.ghostty}/bin/ghostty";
      };
    };
    settings = {
      Keywords = "terminal;tty;pty;";

      X-GNOME-UsesNotifications = "true";
      X-TerminalArgExec = "-e";
      X-TerminalArgTitle = "--title=";
      X-TerminalArgAppId = "--class=";
      X-TerminalArgDir = "--working-directory=";
      X-TerminalArgHold = "--wait-after-command";
    };
  };
}
