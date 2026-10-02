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
  # Set through kdeglobals below: workspace.iconTheme runs Nixpkgs'
  # plasma-changeicons, which pulls a second Plasma and KWin into the profile.
  iconTheme = "breeze-dark";
  cursor = {
    theme = "breeze_cursors";
    size = 48;
  };
  # Devices that move between machines. KWin only applies an entry to a
  # connected device, so declaring them everywhere is harmless.
  sharedTouchpads = [
    # Over Bluetooth the trackpad reports Apple's Bluetooth vendor ID (004c);
    # over USB it reports 05ac. KWin keys settings on both.
    {
      name = "Apple Inc. Magic Trackpad";
      vendorId = "004c";
      productId = "0265";
      pointerSpeed = 1.0;
    }
    {
      name = "Apple Inc. Magic Trackpad";
      vendorId = "05ac";
      productId = "0265";
      pointerSpeed = 1.0;
    }
  ];
  sharedMice = [
    {
      name = "Logitech MX Vertical";
      vendorId = "046d";
      productId = "407b";
      acceleration = 1.0;
      naturalScroll = true;
      scrollSpeed = 2;
    }
  ];
  touchpads = machine.touchpads ++ sharedTouchpads;
  mice = machine.mice ++ sharedMice;
  # Dolphin view properties, in the .directory format of Dolphin 25.12
  # (src/views/viewproperties.cpp). With GlobalViewProps off, a folder
  # without saved settings of its own falls back to view_properties/global;
  # nothing is inherited from parent folders. VisibleRoles is read in
  # order, so it sets the column order.
  #
  # Dolphin ignores a folder's Downloads settings in favour of its own
  # Downloads defaults when Timestamp is older than dolphinrc's
  # ViewPropsTimestamp, and a missing Timestamp counts as older; the
  # declared one is later than any real ViewPropsTimestamp.
  dolphinView = {
    Dolphin = {
      Version = 4;
      Timestamp = "2030,1,1,0,0,0";
      ViewMode = 1; # details
      VisibleRoles =
        lib.concatMapStringsSep "," (role: "Details_${role}") [
          "text"
          "size"
          "creationtime"
          "accesstime"
          "modificationtime"
          "type"
          "extension"
          "permissions"
        ]
        + ",CustomizedDetails";
      SortRole = "type";
      SortOrder = 0; # ascending
      SortFoldersFirst = true;
      SortHiddenLast = true;
      GroupedSorting = false;
      PreviewsShown = false;
    };
    Settings.HiddenFilesShown = true;
  };
in {
  config = {
    programs.plasma = {
      enable = true;
      overrideConfig = false;
      immutableByDefault = false;

      panels = lib.optionals profile.managePlasmaPanels (import ./panels.nix);

      # Plasma copies these to GTK itself (settings.ini and
      # org.gnome.desktop.interface). Home Manager's gtk module would make
      # settings.ini a store link and fight that sync.
      workspace = {
        lookAndFeel = "org.kde.breezedark.desktop";
        inherit cursor;
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
        inherit touchpads;
        mice = map (m: removeAttrs m ["scrollMethod"]) mice;
      };

      # plasma-manager has no scroll-method option for mice.
      configFile.kcminputrc = lib.mkMerge (map (m: {
        "Libinput/${toString (lib.fromHexString m.vendorId)}/${toString (lib.fromHexString m.productId)}/${m.name}".ScrollMethod = m.scrollMethod;
      }) (lib.filter (m: m ? scrollMethod) mice));

      kwin.edgeBarrier = 500;

      powerdevil = {
        AC = {
          autoSuspend.action = "nothing";
          displayBrightness = 100;
          keyboardBrightness = 100;
          powerProfile = "performance";
        };
        battery = {
          displayBrightness = 30;
          keyboardBrightness = 50;
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
          Icons.Theme = iconTheme;
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
          # Dolphin's Previews page keeps the remote limits in kdeglobals,
          # where KIO's preview job reads them; MaximumRemoteSize is in
          # bytes, so 0 skips every remote file.
          PreviewSettings = {
            EnableRemoteFolderThumbnail = false;
            MaximumRemoteSize = 0;
          };
        };

        kwinrc = {
          # Night light, written as keys rather than through kwin.nightLight:
          # that option leaves every key it does not set as null, and
          # plasma-manager deletes null keys on activation, so the saved Mode
          # was dropped on every switch. Its mode choices also predate Plasma
          # 6.5, where kwinrc keeps only Constant or DarkLight and
          # knighttimed owns the schedule.
          NightColor = {
            Active = true;
            Mode = "DarkLight";
            DayTemperature = 6500;
            NightTemperature = 3200;
          };
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
          # KWin launches whichever Fcitx the profile provides.
          Wayland = {
            InputMethod = {
              value =
                if profile.hostFcitx
                then "/usr/share/applications/fcitx5-wayland-launcher.desktop"
                else "${config.home.profileDirectory}/share/applications/fcitx5-wayland-launcher.desktop";
              shellExpand = true;
            };
            VirtualKeyboardEnabled = true;
          };
          Windows.ElectricBorderDelay = 50;
          Xwayland.Scale = machine.xwaylandScale;
        };

        kiorc = {
          Confirmations = {
            ConfirmDelete = true;
            ConfirmEmptyTrash = true;
            ConfirmTrash = true;
          };
          "Executable scripts".behaviourOnLaunch = "alwaysAsk";
        };

        dolphinrc = {
          DetailsMode.IconSize = 22;
          # Folders keep their own view settings, so Downloads can sort
          # differently from the global view declared in dataFile below.
          General.GlobalViewProps = false;
          InformationPanel.showHovered = false;
          "KFileDialog Settings" = {
            "Places Icons Auto-resize" = false;
            "Places Icons Static Size" = 22;
          };
          MainWindow.MenuBar = "Disabled";
          "MainWindow/Toolbar mainToolBar".ToolButtonStyle = "TextUnderIcon";
          # The thumbnailers Dolphin may use, from schan. A pinned list
          # leaves out any thumbnailer installed later until it is added
          # here.
          PreviewSettings.Plugins = lib.concatStringsSep "," [
            "audiothumbnail"
            "blenderthumbnail"
            "comicbookthumbnail"
            "cursorthumbnail"
            "directorythumbnail"
            "djvuthumbnail"
            "ebookthumbnail"
            "exrthumbnail"
            "ffmpegthumbs"
            "fontthumbnail"
            "gsthumbnail"
            "imagethumbnail"
            "jpegthumbnail"
            "kraorathumbnail"
            "mobithumbnail"
            "opendocumentthumbnail"
            "rawthumbnail"
            "svgthumbnail"
            "windowsexethumbnail"
            "windowsimagethumbnail"
          ];
          Search.Location = "Everywhere";
        };

        okularpartrc = {
          "Core Performance".TextHinting = "Enabled";
          "Main View".ShowLeftPanel = false;
          PageView.MouseMode = "TextSelect";
        };

        kded5rc."Module-device_automounter".autoload = false;

        # Sunset to sunrise at the location geoclue reports, falling back to
        # knighttimed's fixed 06:00 and 18:00 when no location is available.
        knighttimerc = {
          General.Source = "Location";
          Location.Automatic = true;
        };

        plasma-localerc.Formats.LANG = "en_US.UTF-8";
      };

      # Written in place, like configFile; activation resets the declared
      # keys. Dolphin stores a folder's own changes in its
      # user.kde.fm.viewproperties#1 extended attribute where the filesystem
      # supports one, and then clears that folder's .directory groups.
      # Downloads assumes the default XDG download directory, ~/Downloads.
      dataFile."dolphin/view_properties/global/.directory" = dolphinView;
      file."Downloads/.directory" = lib.recursiveUpdate dolphinView {
        Dolphin = {
          SortRole = "creationtime";
          SortOrder = 1; # newest first
        };
      };
    };
  };
}
