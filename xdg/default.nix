{
  config,
  lib,
  pkgs,
  username,
  ...
}: let
  solaarWrapped = config.lib.nixGL.wrap pkgs.solaar;
in {
  xdg.enable = true;
  xdg.mime.enable = true;
  targets.genericLinux.enable = true;

  home.packages = [solaarWrapped];

  xdg.configFile."autostart/solaar.desktop" = lib.mkIf (username == "schan") {
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
