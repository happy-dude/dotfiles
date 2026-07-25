{
  homes,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  provider = pkgs.python3Packages.bgutil-ytdlp-pot-provider;
  pluginRoot = "${provider}/${pkgs.python3.sitePackages}";
  serverHome = "${provider}/share/bgutil-ytdlp-pot-provider";
  schanConfig = homes.schan.config.xdg.configFile."yt-dlp/config".source;
  stachanConfig = homes.stachan.config.xdg.configFile."yt-dlp/config".source;
  schanPlugin = homes.schan.config.xdg.configFile."yt-dlp/plugins/bgutil".source;
  stachanPlugin = homes.stachan.config.xdg.configFile."yt-dlp/plugins/bgutil".source;
in {
  yt-dlp-bgutil = assert schanPlugin == pluginRoot;
  assert stachanPlugin == pluginRoot;
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

        cmp ${schanConfig} ${stachanConfig}
        grep -F -- '--no-js-runtimes' ${stachanConfig}
        grep -F -- '--js-runtimes "node:${pkgs.nodejs}/bin/node"' \
          ${stachanConfig}
        grep -F -- \
          '--extractor-args "youtubepot-bgutilscript:server_home=${serverHome}"' \
          ${stachanConfig}
      '';
    };
}
