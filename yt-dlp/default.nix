{pkgs, ...}: let
  bgutilPackage = pkgs.python3Packages.bgutil-ytdlp-pot-provider;
  serverHome = "${bgutilPackage}/share/bgutil-ytdlp-pot-provider";
in {
  programs.yt-dlp = {
    enable = true;
    # The module writes ~/.config/yt-dlp/config itself once this is set;
    # declaring the file beside it would collide.
    extraConfig =
      builtins.readFile ./.config/yt-dlp/config
      + ''
        --no-js-runtimes
        --js-runtimes "node:${pkgs.nodejs}/bin/node"
        --extractor-args "youtubepot-bgutilscript:server_home=${serverHome}"
      '';
  };

  xdg.configFile."yt-dlp/plugins/bgutil".source = "${bgutilPackage}/${pkgs.python3.sitePackages}";
}
