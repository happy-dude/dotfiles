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

    # Kimpanel replaces the themed Fcitx candidate window with a GNOME Shell
    # panel. Keep it disabled unless GNOME Shell integration is more important
    # than Classic UI theming. Verify the package metadata supports the host
    # Shell version before enabling it.
    # programs.gnome-shell = {
    #   enable = true;
    #   extensions = [{package = pkgs.gnomeExtensions.kimpanel;}];
    # };

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
