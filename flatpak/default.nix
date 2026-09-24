{config, ...}: {
  # Flatpak sandboxes see host fonts only from /usr/share/fonts,
  # ~/.local/share/fonts, and ~/.fonts, not the Home Manager profile, so fonts
  # from fonts/default.nix (Noto CJK included) render as tofu in sandboxed
  # apps. This links the profile's whole font tree into the user font
  # directory. Its entries point into /nix/store, which each app must be
  # allowed to read:
  #   flatpak override --user --filesystem=/nix/store:ro [APP]
  # That permission stays out of services.flatpak.overrides for the reason
  # given below.
  xdg.dataFile."fonts/home-manager".source = "${config.home.path}/share/fonts";

  services.flatpak = {
    enable = true;

    packages = [
      "com.interversehq.qView"
      "com.sleepfiles.OSCAR"
      "com.transmissionbt.Transmission"
      "com.valvesoftware.Steam"
      "dev.zed.Zed-Preview"
      "io.github.ungoogled_software.ungoogled_chromium"
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
