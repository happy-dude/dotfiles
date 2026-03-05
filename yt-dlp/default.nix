{ config, pkgs, ... }:

let
  bgutil = pkgs.python314Packages.bgutil-ytdlp-pot-provider;
in
{
  programs.yt-dlp = {
    enable = true;
  };

  xdg.configFile."yt-dlp/config".source = ./.config/yt-dlp/config;
  xdg.configFile."yt-dlp/plugins/bgutil".source = "${bgutil}/${pkgs.python314.sitePackages}";
}
