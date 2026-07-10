{
  config,
  pkgs,
  ...
}: {
  programs.tealdeer = {
    enable = true;
    enableAutoUpdates = false;
    settings = builtins.fromTOML (builtins.readFile ./.config/tealdeer/config.toml);
  };
}
