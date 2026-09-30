# Plasma preferences for every Plasma profile. schan's session is the
# reference; hardware and display values are per machine in ./machines.nix.
#
# plasma-manager writes only the declared keys (overrideConfig = false), so
# anything else changed in System Settings survives activation.
{
  config,
  lib,
  ...
}: let
  profile = config.dotfiles.profile;
  machine = (import ./machines.nix).${profile.username};
in {
  config = {
    programs.plasma = {
      enable = true;
      overrideConfig = false;
      immutableByDefault = false;

      panels = lib.optionals profile.managePlasmaPanels (import ./panels.nix);

      workspace = {
        lookAndFeel = "org.kde.breezedark.desktop";
        cursor.size = 48;
      };

      input = {
        keyboard = {
          numlockOnStartup = "on";
          layouts = [
            {
              layout = "us";
              variant = "colemak";
            }
          ];
        };
        touchpads = machine.touchpads;
        mice = map (m: removeAttrs m ["scrollMethod"]) machine.mice;
      };

      # plasma-manager has no scroll-method option for mice.
      configFile.kcminputrc = lib.mkMerge (map (m: {
        "Libinput/${toString (lib.fromHexString m.vendorId)}/${toString (lib.fromHexString m.productId)}/${m.name}".ScrollMethod = m.scrollMethod;
      }) (lib.filter (m: m ? scrollMethod) machine.mice));

      kwin = {
        edgeBarrier = 500;
        nightLight = {
          enable = true;
          temperature.night = 3200;
        };
      };

      powerdevil = {
        AC = {
          autoSuspend.action = "nothing";
          displayBrightness = 100;
          powerProfile = "performance";
        };
        battery = {
          displayBrightness = 30;
          powerProfile = "balanced";
        };
        lowBattery = {
          displayBrightness = 10;
          keyboardBrightness = 0;
          powerProfile = "powerSaving";
        };
      };

      shortcuts = {
        "KDE Keyboard Layout Switcher" = {
          "Switch to Last-Used Keyboard Layout" = "Meta+Alt+L";
          "Switch to Next Keyboard Layout" = "Meta+Alt+K";
        };
        kwin = {
          # Replaces the Meta+F<n> bindings for these actions.
          "Expose" = "Ctrl+F9";
          "ExposeAll" = ["Launch (C)" "Ctrl+F10"];
          "ExposeClass" = "Ctrl+F7";
          "Switch to Desktop 1" = "Ctrl+F1";
          "Switch to Desktop 2" = "Ctrl+F2";
          "Switch to Desktop 3" = "Ctrl+F3";
          "Switch to Desktop 4" = "Ctrl+F4";
          "Window Move Center" = "Meta+C";
          "Window to Next Screen" = "Meta+Shift+Right";
          "Window to Previous Screen" = "Meta+Shift+Left";
        };
        org_kde_powerdevil.powerProfile = [
          "Battery"
          "Meta+B"
        ];
        plasmashell = {
          "manage activities" = "Meta+Q";
          "next activity" = "Meta+A";
          "previous activity" = "Meta+Shift+A";
        };
      };

      configFile = {
        kdeglobals = {
          General = {
            AccentColor = "248,108,0";
            LastUsedCustomAccentColor = "248,108,0";
            TerminalService = "com.mitchellh.ghostty.desktop";
          };
          KDE = {
            LookAndFeelPackage = "org.kde.breezedark.desktop";
            contrast = 4;
            frameContrast = 0.2;
          };
          "KFileDialog Settings" = {
            "Allow Expansion" = false;
            "Automatically select filename extension" = true;
            "Breadcrumb Navigation" = false;
            "Decoration position" = 2;
            "Show Full Path" = false;
            "Show Inline Previews" = true;
            "Show Preview" = false;
            "Show Speedbar" = true;
            "Show hidden files" = true;
            "Sort by" = "Date";
            "Sort directories first" = true;
            "Sort hidden files last" = false;
            "Sort reversed" = false;
            "Speedbar Width" = 154;
            "View Style" = "DetailTree";
          };
        };

        kwinrc = {
          Desktops = {
            Number = 2;
            Rows = 1;
          };
          "Effect-windowview".BorderActivateClass = 5;
          ElectricBorders = {
            BottomRight = "LockScreen";
            TopRight = "ShowDesktop";
          };
          Plugins.zoomEnabled = false;
          TabBox = {
            OrderMinimizedMode = 1;
            ShowDesktopMode = 1;
          };
          TabBoxAlternative = {
            OrderMinimizedMode = 1;
            ShowDesktopMode = 1;
          };
          Wayland = {
            InputMethod = {
              value = "/usr/share/applications/fcitx5-wayland-launcher.desktop";
              shellExpand = true;
            };
            VirtualKeyboardEnabled = true;
          };
          Windows.ElectricBorderDelay = 50;
          Xwayland.Scale = machine.xwaylandScale;
        };

        kiorc.Confirmations = {
          ConfirmDelete = true;
          ConfirmEmptyTrash = true;
        };

        dolphinrc = {
          DetailsMode.IconSize = 22;
          InformationPanel.showHovered = false;
          "KFileDialog Settings" = {
            "Places Icons Auto-resize" = false;
            "Places Icons Static Size" = 22;
          };
          MainWindow.MenuBar = "Disabled";
          "MainWindow/Toolbar mainToolBar".ToolButtonStyle = "TextUnderIcon";
          Search.Location = "Everywhere";
        };

        okularpartrc = {
          "Core Performance".TextHinting = "Enabled";
          "Main View".ShowLeftPanel = false;
          PageView.MouseMode = "TextSelect";
        };

        kded5rc."Module-device_automounter".autoload = false;

        plasma-localerc.Formats.LANG = "en_US.UTF-8";
      };
    };
  };
}
