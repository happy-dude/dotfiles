{
  homes,
  lib,
  pkgs,
}: let
  inherit (homes) schan stachan;
  stachanConfig = stachan.config.xdg.configFile."opencode/opencode.json".source;
  schanConfig = schan.config.xdg.configFile."opencode/opencode.json".source;
  languageServerPackages = with pkgs; [
    eslint
    haskell-language-server
    kotlin-language-server
    nixd
    oxlint
    terraform-ls
    typescript
    typescript-language-server
    vscode-langservers-extracted
    yaml-language-server
  ];
  stachanTui = stachan.config.xdg.configFile."opencode/tui.json".source;
  schanTui = schan.config.xdg.configFile."opencode/tui.json".source;
  stachanTheme = stachan.config.xdg.configFile."opencode/themes/gruvbox-material.json".source;
  schanTheme = schan.config.xdg.configFile."opencode/themes/gruvbox-material.json".source;
  stachanPackage =
    lib.findFirst (
      package: lib.hasPrefix "opencode-no-telemetry-" package.name
    )
    null
    stachan.config.home.packages;
  schanPackage =
    lib.findFirst (
      package: lib.hasPrefix "opencode-no-telemetry-" package.name
    )
    null
    schan.config.home.packages;
in
  assert lib.all (package: lib.elem package stachan.config.home.packages) languageServerPackages;
  assert lib.all (package: lib.elem package schan.config.home.packages) languageServerPackages;
  assert stachan.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true";
  assert schan.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true";
  assert stachanPackage != null;
  assert schanPackage != null;
  assert stachan.config.home.sessionVariables.OPENCODE_CONFIG == "/home/stachan/.config/opencode/local.json";
  assert schan.config.home.sessionVariables.OPENCODE_CONFIG == "/home/schan/.config/opencode/local.json";
    pkgs.runCommand "dotfiles-opencode-check"
    {
      nativeBuildInputs = [
        pkgs.check-jsonschema
        pkgs.jq
        stachanPackage
      ];
    }
    ''
      export HOME="$PWD/home"
      export OPENCODE_CONFIG="$HOME/.config/opencode/local.json"
      export OTEL_EXPORTER_OTLP_ENDPOINT=https://telemetry.invalid
      export OTEL_EXPORTER_OTLP_HEADERS=authorization=test-only
      export OTEL_RESOURCE_ATTRIBUTES=service.namespace=dotfiles-test
      export XDG_CACHE_HOME="$HOME/.cache"
      export XDG_CONFIG_HOME="$HOME/.config"
      export XDG_DATA_HOME="$HOME/.local/share"
      export XDG_STATE_HOME="$HOME/.local/state"
      mkdir -p "$XDG_CONFIG_HOME/opencode"
      grep -F 'unset OTEL_EXPORTER_OTLP_ENDPOINT' \
        "$(command -v opencode)"
      grep -F 'unset OTEL_EXPORTER_OTLP_HEADERS' \
        "$(command -v opencode)"
      grep -F 'unset OTEL_RESOURCE_ATTRIBUTES' \
        "$(command -v opencode)"
      cmp ${stachanConfig} ${schanConfig}
      cmp ${stachanTui} ${schanTui}
      cmp ${stachanTheme} ${schanTheme}
      check-jsonschema \
        --schemafile ${stachanPackage}/share/opencode/tui.json \
        ${stachanTui}
      check-jsonschema \
        --schemafile ${pkgs.opencode.src}/packages/web/public/theme.json \
        ${stachanTheme}
      jq -e '
        .theme == "gruvbox-material"
      ' ${stachanTui} >/dev/null
      jq -e '
        .defs.bg0 == "#282828" and
        .defs.fg0 == "#d4be98" and
        .defs.red == "#ea6962" and
        .defs.green == "#a9b665" and
        .defs.blue == "#7daea3" and
        .defs.diffRed == "#402120" and
        .defs.diffGreen == "#34381b" and
        .theme.background == "bg0" and
        .theme.text == "fg0" and
        .theme.diffAddedBg == "diffGreen" and
        .theme.diffRemovedBg == "diffRed" and
        (.theme | length) >= 50
      ' ${stachanTheme} >/dev/null
      install -m 0600 ${stachanConfig} \
        "$XDG_CONFIG_HOME/opencode/opencode.json"

      opencode debug config >resolved.json
      jq -e '
        .share == "disabled" and
        .autoupdate == false and
        .experimental.openTelemetry == false and
        .permission.bash == "ask" and
        .permission.external_directory == "ask" and
        .permission.lsp == "allow" and
        .lsp.eslint.command == ["vscode-eslint-language-server", "--stdio"] and
        .lsp["kotlin-ls"].command == ["kotlin-language-server"] and
        .lsp.oxlint.disabled == true and
        .lsp.typescript.initialization.tsserver.path ==
          "${pkgs.typescript}/lib/node_modules/typescript/lib/tsserver.js" and
        .lsp["fish-lsp"].command == ["fish-lsp", "start"] and
        .lsp.zuban.command == ["zuban", "server"] and
        (.model == null) and
        (.enabled_providers == null) and
        .agent.kernel.mode == "all" and
        .agent.language.mode == "all"
      ' resolved.json >/dev/null
      touch "$out"
    ''
