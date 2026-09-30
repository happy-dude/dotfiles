# schan's panel, applied where the profile sets managePlasmaPanels.
# plasma-manager rebuilds plasma-org.kde.plasma.desktop-appletsrc whenever
# this changes, discarding panel edits made in the session.
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
          favoritesDisplayMode = "list";
          showButtonsFor.custom = [
            "suspend"
            "hibernate"
            "reboot"
            "shutdown"
          ];
          settings.General.highlightNewlyInstalledApps = false;
        };
      }
      "org.kde.plasma.pager"
      {
        panelSpacer.expanding = true;
      }
      {
        iconTasks = {
          launchers = [
            "preferred://filemanager"
            "applications:com.mitchellh.ghostty.desktop"
            "applications:firefox-nightly.desktop"
            "applications:org.mozilla.thunderbird.desktop"
          ];
          behavior.showTasks.onlyInCurrentDesktop = false;
          settings.General.wheelEnabled = "TaskOnly";
        };
      }
      {
        panelSpacer.expanding = true;
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
      {
        digitalClock.font = null;
        digitalClock.settings.Appearance.fontWeight = 400;
      }
      "org.kde.plasma.showdesktop"
    ];
  }
]
