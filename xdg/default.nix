{
  config,
  lib,
  pkgs,
  ...
}: let
  solaarWrapped = config.lib.nixGL.wrap pkgs.solaar;
in {
  xdg.enable = true;
  xdg.mime.enable = true;
  targets.genericLinux.enable = true;

  # Solaar only manages a Logitech receiver; install it only where one is
  # attached, matching the autostart entry below.
  home.packages = lib.optionals config.dotfiles.profile.hasSolaar [solaarWrapped];

  xdg.configFile."autostart/solaar.desktop" = lib.mkIf config.dotfiles.profile.hasSolaar {
    text = ''
      [Desktop Entry]
      Type=Application
      Name=Solaar
      Comment=Logitech Unifying Receiver peripherals manager
      Exec=${solaarWrapped}/bin/solaar --window=hide
      Icon=solaar
      Terminal=false
      StartupNotify=false
      X-GNOME-UsesNotifications=true
    '';
  };
}
