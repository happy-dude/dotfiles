{
  services.flatpak = {
    enable = true;

    packages = [
      "com.interversehq.qView"
      "com.sleepfiles.OSCAR"
      "com.transmissionbt.Transmission"
      "com.valvesoftware.Steam"
      "dev.zed.Zed-Preview"
      "org.audacityteam.Audacity"
      "org.kde.okular"
      "org.libreoffice.LibreOffice"
      "org.mozilla.thunderbird"
      "org.mozilla.vpn"
      "org.videolan.VLC"
      "us.zoom.Zoom"
      {
        appId = "org.mozilla.FirefoxNightly";
        flatpakref = "https://gitlab.com/projects261/firefox-nightly-flatpak/-/raw/f68f2869085856c44db68c0863af469cdb2d258b/firefox-nightly.flatpakref";
        sha256 = "sha256-qVbNliiJTrjVxUDMjq6QS4ZIcXrSFx7Gc9P/NR4rc9U=";
      }
    ];

    uninstallUnmanaged = false;
    uninstallUnused = false;

    update = {
      onActivation = false;
      auto.enable = false;
    };

    overrides = {};
  };
}
