{...}: {
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
    ];

    # nix-flatpak v0.7.0 can introduce leading empty permissions while merging
    # externally managed list entries, so keep overrides outside this module.
    overrides = {};
  };
}
