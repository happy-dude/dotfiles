# Fcitx 5 and its Rime addon on hosts that do not install them. Rime data
# comes from rime/ everywhere.
#
# GNOME needs the systemd unit and QT_IM_MODULE. KWin starts Fcitx itself as
# its Wayland input method, so a unit would run a second daemon and IM-module
# variables would bypass it.
# ref: <https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland#KDE_Plasma>
{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.dotfiles.profile) desktop hostFcitx;
in {
  config = lib.mkIf (!hostFcitx) {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        waylandFrontend = true;
        systemd.enable = desktop == "gnome";
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
        HiddenNotifications=${
          if desktop == "plasma"
          then "wayland-diagnose-kde"
          else "wayland-diagnose-gnome"
        }
      '';
    };

    # Home Manager omits this for the Wayland frontend; GNOME Qt apps need it.
    home.sessionVariables = lib.mkIf (desktop == "gnome") {
      QT_IM_MODULE = "fcitx";
    };
  };
}
