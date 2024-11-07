{ config, pkgs, ... }:

{
  programs.wezterm = {
    enable = true;
    package = pkgs.wezterm;
    extraConfig = ''
      ${builtins.readFile ./.config/wezterm/wezterm.lua}
    '';
  };
}
