{ config, pkgs, ... }:

{
  # wezterm installed via home-manager errors on Ubuntu 24.10
  # need to investigate further; until then, install via PPA and only link config

  xdg.configFile."wezterm/wezterm.lua".source = ./.config/wezterm/wezterm.lua;

  programs.wezterm = {
    enable = false;
    package = (config.lib.nixGL.wrap pkgs.wezterm);
    extraConfig = ''
      ${builtins.readFile ./.config/wezterm/wezterm.lua}
    '';
  };
}
