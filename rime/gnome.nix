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

    home.packages = [pkgs.gnomeExtensions.custom-hot-corners-extended];

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        cursor-size = 96;
        document-font-name = "Noto Serif 11";
        enable-animations = true;
        font-name = "Ubuntu 11";
        gtk-key-theme = "Emacs";
        icon-theme = "Yaru";
        locate-pointer = false;
        monospace-font-name = "Noto Sans Mono 13";
        show-battery-percentage = true;
        text-scaling-factor = 1.25;
        toolkit-accessibility = false;
      };
      "org/gnome/shell/extensions/custom-hot-corners-extended/misc" = {
        action-event-delay = 50;
        show-osd-monitor-indexes = false;
        supported-active-extensions =
          lib.hm.gvariant.mkEmptyArray lib.hm.gvariant.type.string;
      };
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-bottom-left-0".action = "toggle-overview-app";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-bottom-left-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-bottom-right-0".action = "lock-screen";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-bottom-right-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-left-0".action = "show-overview";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-left-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-right-0".action = "show-desktop-mon";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-0-top-right-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-bottom-left-0".action = "toggle-overview-app";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-bottom-left-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-bottom-right-0".action = "lock-screen";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-bottom-right-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-top-left-0".action = "show-overview";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-top-left-6".ctrl = true;
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-top-right-0".action = "show-desktop-mon";
      "org/gnome/shell/extensions/custom-hot-corners-extended/monitor-1-top-right-6".ctrl = true;
    };

    # Home Manager omits this for the Wayland frontend; GNOME Qt apps need it.
    home.sessionVariables.QT_IM_MODULE = "fcitx";
  };
}
