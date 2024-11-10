{ config, pkgs, ... }:

{
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    fira-code
    fira-code-nerdfont
  ];
}
