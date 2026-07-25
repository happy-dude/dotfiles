{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  provider = pkgs.python3Packages.bgutil-ytdlp-pot-provider;
  pluginRoot = "${provider}/${pkgs.python3.sitePackages}";
  serverHome = "${provider}/share/bgutil-ytdlp-pot-provider";
  config = shared homes "the yt-dlp configuration" (
    home: home.config.xdg.configFile."yt-dlp/config".source
  );
  plugin = shared homes "the bgutil plugin directory" (
    home: home.config.xdg.configFile."yt-dlp/plugins/bgutil".source
  );
in {
  yt-dlp-bgutil = assert plugin == pluginRoot;
    mkCheck {
      name = "yt-dlp-bgutil-check";
      tools = [pkgs.nodejs];
      script = ''
        test -d ${pluginRoot}/yt_dlp_plugins
        test -f ${serverHome}/build/generate_once.js
        test "$(
          node ${serverHome}/build/generate_once.js --version
        )" = ${provider.version}
        ${provider}/bin/bgutil-ytdlp-pot-provider --help >/dev/null

        grep -F -- '--no-js-runtimes' ${config}
        grep -F -- '--js-runtimes "node:${pkgs.nodejs}/bin/node"' \
          ${config}
        grep -F -- \
          '--extractor-args "youtubepot-bgutilscript:server_home=${serverHome}"' \
          ${config}
      '';
    };
}
