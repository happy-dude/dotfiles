{
  inputs,
  pkgs,
  lib,
  ...
}:

let
  bgutilProvider = inputs.bgutil_ytdlp_pot_provider;
  bgutilPackageJson = builtins.fromJSON (builtins.readFile "${bgutilProvider}/server/package.json");
  bgutilPackage = pkgs.buildNpmPackage {
    pname = "bgutil-ytdlp-pot-provider";
    inherit (bgutilPackageJson) version;

    src = "${bgutilProvider}/server";
    npmDepsHash = "sha256-Qwwi6W+Oeu6ZeLmZP5vEfAKOJyivbULR5mlk7tcVIE8=";

    nativeBuildInputs = [ pkgs.pkg-config ];
    buildInputs = with pkgs; [
      cairo
      giflib
      libjpeg
      libpng
      librsvg
      pango
      pixman
    ];

    # canvas' prebuilt binary is not part of package-lock.json. Build it from
    # source so this derivation never downloads artifacts during npm rebuild.
    npmRebuildFlags = [ "--build-from-source" ];
    npmPruneFlags = [ "--ignore-scripts" ];

    # Upstream documents `npx tsc` but has no package.json build script.
    buildPhase = ''
      runHook preBuild
      node_modules/.bin/tsc
      runHook postBuild
    '';

    postInstall = ''
      mkdir -p "$out/share/yt-dlp/plugins/bgutil"
      cp -r "${bgutilProvider}/plugin/." "$out/share/yt-dlp/plugins/bgutil/"
    '';

    doInstallCheck = true;
    installCheckPhase = ''
      runHook preInstallCheck
      test "$(
        "${pkgs.nodejs}/bin/node" \
          "$out/lib/node_modules/bgutil-ytdlp-pot-provider/build/generate_once.js" \
          --version
      )" = "${bgutilPackageJson.version}"
      runHook postInstallCheck
    '';

    meta = {
      description = "Proof-of-origin token provider plugin for yt-dlp";
      homepage = "https://github.com/Brainicism/bgutil-ytdlp-pot-provider";
      license = lib.licenses.gpl3Only;
    };
  };

  serverHome = "${bgutilPackage}/lib/node_modules/bgutil-ytdlp-pot-provider";
in
{
  programs.yt-dlp.enable = true;

  xdg.configFile."yt-dlp/plugins/bgutil".source = "${bgutilPackage}/share/yt-dlp/plugins/bgutil";

  xdg.configFile."yt-dlp/config".text = builtins.readFile ./.config/yt-dlp/config + ''
    --no-js-runtimes
    --js-runtimes "node:${pkgs.nodejs}/bin/node"
    --extractor-args "youtubepot-bgutilscript:server_home=${serverHome}"
  '';
}
