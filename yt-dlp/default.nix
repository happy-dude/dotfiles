{pkgs, ...}: let
  bgutilPackage = pkgs.python3Packages.bgutil-ytdlp-pot-provider;
  serverHome = "${bgutilPackage}/share/bgutil-ytdlp-pot-provider";
in {
  programs.yt-dlp.enable = true;

  xdg.configFile."yt-dlp/plugins/bgutil".source = "${bgutilPackage}/${pkgs.python3.sitePackages}";

  xdg.configFile."yt-dlp/config".text =
    builtins.readFile ./.config/yt-dlp/config
    + ''
      --no-js-runtimes
      --js-runtimes "node:${pkgs.nodejs}/bin/node"
      --extractor-args "youtubepot-bgutilscript:server_home=${serverHome}"
    '';
}
