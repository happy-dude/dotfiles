{
  services.flatpak = {
    enable = true;

    packages = [
      "com.sleepfiles.OSCAR"
      "com.transmissionbt.Transmission"
      "com.valvesoftware.Steam"
      "dev.edfloreshz.CosmicTweaks"
      "dev.zed.Zed-Preview"
      "org.audacityteam.Audacity"
      "org.kde.okular"
      "org.libreoffice.LibreOffice"
      "org.mozilla.thunderbird"
      "org.mozilla.vpn"
      "org.videolan.VLC"
      "us.zoom.Zoom"
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
