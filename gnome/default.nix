{
  config,
  lib,
  pkgs,
  desktop,
  ...
}: let
  cursorTheme = "Yaru";
  cursorSize = 96;
in {
  config = lib.mkIf (config.dotfiles.profile.desktop == "gnome") {
    # Kimpanel replaces the themed Fcitx candidate window with a GNOME Shell
    # panel. Keep it disabled unless GNOME Shell integration is more important
    # than Classic UI theming. Verify the package metadata supports the host
    # Shell version before enabling it.
    # programs.gnome-shell = {
    #   enable = true;
    #   extensions = [{package = pkgs.gnomeExtensions.kimpanel;}];
    # };

    home.packages = [pkgs.gnomeExtensions.custom-hot-corners-extended];

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        cursor-size = cursorSize;
        cursor-theme = cursorTheme;
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
      "org/gnome/desktop/input-sources".sources = [
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "us+colemak"
        ])
        (lib.hm.gvariant.mkTuple [
          "xkb"
          "us"
        ])
      ];
      "org/gnome/desktop/peripherals/mouse" = {
        natural-scroll = true;
        speed = 1.0;
      };
      "org/gnome/desktop/peripherals/touchpad" = {
        speed = 1.0;
        two-finger-scrolling-enabled = true;
      };
      "org/gnome/nautilus/list-view" = {
        default-column-order = [
          "name"
          "size"
          "type"
          "owner"
          "group"
          "permissions"
          "date_modified"
          "date_accessed"
          "date_created"
          "recency"
          "detailed_type"
        ];
        default-visible-columns = [
          "name"
          "size"
          "permissions"
          "date_modified"
          "detailed_type"
        ];
      };
      "org/gnome/nautilus/preferences" = {
        default-folder-viewer = "list-view";
        search-filter-time-type = "last_modified";
      };
      "org/gnome/settings-daemon/plugins/power" = {
        sleep-inactive-ac-timeout = 3600;
        sleep-inactive-ac-type = "nothing";
        sleep-inactive-battery-type = "suspend";
      };
      "org/gnome/shell".favorite-apps = [
        "org.gnome.Nautilus.desktop"
        "com.mitchellh.ghostty.desktop"
        "firefox-nightly.desktop"
      ];
      "org/gnome/shell/extensions/dash-to-dock" = {
        dash-max-icon-size = 64;
        dock-fixed = false;
        dock-position = "BOTTOM";
        extend-height = false;
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
  };
}
