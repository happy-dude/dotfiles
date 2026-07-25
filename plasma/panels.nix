# Captured Plasma panel layout, applied only when
# dotfiles.plasma.managePanels is enabled. plasma-manager rebuilds
# plasma-org.kde.plasma.desktop-appletsrc from scratch whenever this
# declaration changes, discarding panel edits made in the session.
[
  {
    location = "bottom";
    floating = true;
    hiding = "none";
    lengthMode = "fill";
    height = 36;
    widgets = [
      {
        kickoff = {
          compactDisplayStyle = true;
          sortAlphabetically = true;
        };
      }
      "org.kde.plasma.pager"
      {
        panelSpacer = {
          expanding = false;
          length = 120;
        };
      }
      {
        iconTasks = {
          launchers = [
            "preferred://filemanager"
            "applications:com.mitchellh.ghostty.desktop"
            "preferred://browser"
            "applications:org.mozilla.thunderbird.desktop"
          ];
          behavior.showTasks.onlyInCurrentDesktop = false;
        };
      }
      {
        panelSpacer = {
          expanding = false;
          length = 120;
        };
      }
      "org.kde.plasma.marginsseparator"
      {
        systemTray = {
          items = {
            hidden = ["org.kde.plasma.addons.katesessions"];
            shown = [
              "org.kde.plasma.battery"
              "org.kde.plasma.bluetooth"
              "org.kde.plasma.volume"
              "org.kde.plasma.mediacontroller"
              "Fcitx"
              "org.kde.plasma.brightness"
            ];
            configs.battery.showPercentage = true;
          };
        };
      }
      "org.kde.plasma.digitalclock"
      "org.kde.plasma.showdesktop"
    ];
  }
]
