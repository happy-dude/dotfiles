{
  config,
  lib,
  ...
}: {
  options.dotfiles.plasma.managePanels = lib.mkEnableOption ''
    Home Manager ownership of the complete Plasma panel layout
  '';

  config = lib.mkIf (config.dotfiles.profile.desktop == "plasma") {
    programs.plasma = {
      enable = true;
      overrideConfig = false;
      immutableByDefault = false;

      panels = lib.optionals config.dotfiles.plasma.managePanels (import ./panels.nix);

      workspace = {
        lookAndFeel = "org.kde.breezedark.desktop";
        cursor.size = 48;
      };

      input = {
        keyboard.numlockOnStartup = "on";
        touchpads = [
          {
            name = "SNSL002D:00 2C2F:002D Touchpad";
            vendorId = "2c2f";
            productId = "002d";
            pointerSpeed = 0.8;
            naturalScroll = true;
          }
        ];
        mice = [
          {
            name = "SNSL002D:00 2C2F:002D Mouse";
            vendorId = "2c2f";
            productId = "002d";
            acceleration = 1.0;
            naturalScroll = true;
          }
          {
            name = "Logitech MX Vertical";
            vendorId = "046d";
            productId = "407b";
            acceleration = 1.0;
            naturalScroll = true;
            scrollSpeed = 2;
          }
          {
            name = "TPPS/2 Elan TrackPoint";
            vendorId = "0002";
            productId = "000a";
            acceleration = 1.0;
            naturalScroll = true;
          }
        ];
      };

      kwin = {
        edgeBarrier = 500;
        nightLight = {
          enable = true;
          temperature.night = 3200;
        };
      };

      shortcuts = {
        "KDE Keyboard Layout Switcher" = {
          "Switch to Last-Used Keyboard Layout" = "Meta+Alt+L";
          "Switch to Next Keyboard Layout" = "Meta+Alt+K";
        };
        kwin = {
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
          Xwayland.Scale = 1.25;
        };

        kxkbrc.Layout = {
          DisplayNames = "";
          LayoutList = "us";
          Use = true;
          VariantList = "colemak";
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
        };

        plasma-localerc.Formats.LANG = "en_US.UTF-8";
      };
    };
  };
}
