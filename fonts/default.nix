{ config, pkgs, ... }:

{
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      serif = [ "DejaVu Serif" ];
      sansSerif = [ "DejaVu Sans" ];
      monospace = [ "FiraCode Nerd Font" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  home.packages = with pkgs; [
    ubuntu_font_family
    fira
    fira-code
    noto-fonts
    noto-fonts-color-emoji

    nerd-fonts.comic-shanns-mono
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.droid-sans-mono
    nerd-fonts.fira-code
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka-term
    nerd-fonts.noto
  ];
}
