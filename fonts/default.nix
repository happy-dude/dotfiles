{ config, pkgs, ... }:

{
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      serif = [ "Noto Serif" ];
      sansSerif = [ "Noto Sans" ];
      monospace = [ "FantasqueSansM Nerd Font Mono" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  home.packages = with pkgs; [
    atkinson-hyperlegible
    atkinson-hyperlegible-mono
    atkinson-hyperlegible-next
    atkinson-monolegible
    ubuntu_font_family
    fira
    fira-code
    intel-one-mono
    noto-fonts
    noto-fonts-color-emoji

    nerd-fonts.caskaydia-cove
    nerd-fonts.comic-shanns-mono
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.droid-sans-mono
    nerd-fonts.fantasque-sans-mono
    nerd-fonts.fira-code
    nerd-fonts.intone-mono
    nerd-fonts.iosevka-term
    nerd-fonts.jetbrains-mono
    nerd-fonts.meslo-lg
    nerd-fonts.mononoki
    nerd-fonts.noto
    nerd-fonts.ubuntu-mono
    nerd-fonts.zed-mono
  ];
}
