{...}: {
  programs.tealdeer = {
    enable = true;
    # Home Manager's tldr-update timer owns the page cache; the native
    # config keeps auto_update off so a lookup never downloads.
    enableAutoUpdates = true;
    settings = builtins.fromTOML (builtins.readFile ./.config/tealdeer/config.toml);
  };
}
