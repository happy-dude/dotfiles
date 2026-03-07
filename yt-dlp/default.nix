{
  config,
  pkgs,
  lib,
  ...
}:

let
  version = "1.3.1";
  installStamp = "${version}-node";

  bgutilProvider = pkgs.fetchFromGitHub {
    owner = "Brainicism";
    repo = "bgutil-ytdlp-pot-provider";
    rev = version;
    hash = "sha256-dhpataQ1HSCRPnm4k3K/NMaQPQdNrx8C4q855l7kbbQ=";
  };

  providerHome = "${config.home.homeDirectory}/.local/share/bgutil-ytdlp-pot-provider";
  serverHome = "${providerHome}/server";
  npmCacheHome = "${config.home.homeDirectory}/.cache/npm";
in
{
  programs.yt-dlp.enable = true;

  # Option B only needs a JS runtime in PATH.
  # nodejs installed via home.nix

  # Matches the plugin-folder style install the project documents.
  xdg.configFile."yt-dlp/plugins/bgutil".source = "${bgutilProvider}/plugin";

  # Copy the provider's server/ files to a writable location and install deps there.
  # This keeps the Nix store read-only while still letting the provider work.
  home.activation.bgutilYtdlpProvider = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    echo -e "\e[32mChecking bgutil-ytdlp-pot-provider...\e[0m"

    mkdir -p "${npmCacheHome}"

    if [ ! -f "${providerHome}/.installed-state" ] || \
       [ "$(${pkgs.coreutils}/bin/cat "${providerHome}/.installed-state" 2>/dev/null)" != "${installStamp}" ]; then
      echo -e "\e[33mInstalling bgutil-ytdlp-pot-provider ${version} for Node.js\e[0m"

      rm -rf "${providerHome}"
      mkdir -p "${providerHome}"
      cp -r "${bgutilProvider}/server" "${serverHome}"
      chmod -R u+w "${providerHome}"

      (
        export PATH="${pkgs.nodejs}/bin:$PATH"
        export npm_config_cache="${npmCacheHome}"

        cd "${serverHome}"

        echo -e "\e[32mRunning npm ci...\e[0m"
        npm ci

        echo -e "\e[32mTranspiling with TypeScript...\e[0m"
        npm exec tsc
      )

      echo "${installStamp}" > "${providerHome}/.installed-state"
      echo -e "\e[32mInstalled bgutil-ytdlp-pot-provider ${version} for Node.js\e[0m"
    else
      echo -e "\e[32mbgutil-ytdlp-pot-provider ${version} for Node.js already installed\e[0m"
    fi
  '';

  xdg.configFile."yt-dlp/config".text = ''
    ${builtins.readFile ./.config/yt-dlp/config}
        --no-js-runtimes
    --js-runtimes "node:${pkgs.nodejs}/bin/node"
    --extractor-args "youtubepot-bgutilscript:server_home=${serverHome}"
  '';
}
