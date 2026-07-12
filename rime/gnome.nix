{
  lib,
  pkgs,
  desktop,
  ...
}: {
  config = lib.mkIf (desktop == "gnome") {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        waylandFrontend = true;
        addons = with pkgs; [
          fcitx5-rime
          fcitx5-gtk
        ];
      };
    };

    xdg.configFile."fcitx5/conf/notifications.conf" = {
      force = true;
      text = ''
        # Hidden Notifications
        HiddenNotifications=wayland-diagnose-gnome
      '';
    };

    # Home Manager omits this for the Wayland frontend; GNOME Qt apps need it.
    home.sessionVariables.QT_IM_MODULE = "fcitx";
  };
}
