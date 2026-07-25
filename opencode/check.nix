{
  homes,
  lib,
  pkgs,
  self,
}: let
  inherit (homes) schan stachan;
  stachanConfig = stachan.config.xdg.configFile."opencode/opencode.json".source;
  schanConfig = schan.config.xdg.configFile."opencode/opencode.json".source;
  languageServerPackages = with pkgs; [
    bash-language-server
    clojure-lsp
    eslint
    fennel-ls
    fish-lsp
    gopls
    haskell-language-server
    kotlin-language-server
    (lib.lowPrio clang-tools)
    lua-language-server
    marksman
    nixd
    oxlint
    perlnavigator
    ruff
    rust-analyzer
    terraform-ls
    texlab
    tinymist
    typescript
    typescript-language-server
    vim-language-server
    vscode-langservers-extracted
    yaml-language-server
    zls
    zuban
  ];
  stachanTui = stachan.config.xdg.configFile."opencode/tui.json".source;
  schanTui = schan.config.xdg.configFile."opencode/tui.json".source;
  stachanTheme = stachan.config.xdg.configFile."opencode/themes/gruvbox-material.json".source;
  schanTheme = schan.config.xdg.configFile."opencode/themes/gruvbox-material.json".source;
  stachanMixTheme = stachan.config.xdg.configFile."opencode/themes/gruvbox-material-mix-dark-medium.json".source;
  schanMixTheme = schan.config.xdg.configFile."opencode/themes/gruvbox-material-mix-dark-medium.json".source;
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
  assert stachan.config.home.sessionVariables.OPENCODE_CONFIG
  == "${stachan.config.home.homeDirectory}/.config/opencode/local.json";
  assert schan.config.home.sessionVariables.OPENCODE_CONFIG
  == "${schan.config.home.homeDirectory}/.config/opencode/local.json";
    pkgs.runCommand "dotfiles-opencode-check"
    {
      nativeBuildInputs =
        [
          pkgs.check-jsonschema
          pkgs.jq
          stachanPackage
        ]
        ++ languageServerPackages;
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
      for command in \
        bash-language-server \
        clangd \
        clojure-lsp \
        fennel-ls \
        fish-lsp \
        gopls \
        haskell-language-server-wrapper \
        kotlin-language-server \
        lua-language-server \
        marksman \
        nixd \
        perlnavigator \
        ruff \
        rust-analyzer \
        terraform-ls \
        texlab \
        tinymist \
        typescript-language-server \
        vim-language-server \
        vscode-eslint-language-server \
        vscode-json-language-server \
        yaml-language-server \
        zls \
        zuban
      do
        command -v "$command" >/dev/null
      done
      grep -F 'unset OTEL_EXPORTER_OTLP_ENDPOINT' \
        "$(command -v opencode)"
      grep -F 'unset OTEL_EXPORTER_OTLP_HEADERS' \
        "$(command -v opencode)"
      grep -F 'unset OTEL_RESOURCE_ATTRIBUTES' \
        "$(command -v opencode)"
      cmp ${stachanConfig} ${schanConfig}
      cmp ${stachanTui} ${schanTui}
      cmp ${stachanTheme} ${schanTheme}
      cmp ${stachanMixTheme} ${schanMixTheme}
      check-jsonschema \
        --schemafile ${stachanPackage}/share/opencode/tui.json \
        ${stachanTui}
      check-jsonschema \
        --schemafile ${pkgs.opencode.src}/packages/web/public/theme.json \
        ${stachanTheme}
      check-jsonschema \
        --schemafile ${pkgs.opencode.src}/packages/web/public/theme.json \
        ${stachanMixTheme}
      jq -e '
        .theme == "gruvbox-material-mix-dark-medium"
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
      jq -e '
        .defs.bg0 == "#282828" and
        .defs.fg0 == "#e2cca9" and
        .defs.red == "#f2594b" and
        .defs.orange == "#f28534" and
        .defs.yellow == "#e9b143" and
        .defs.green == "#b0b846" and
        .defs.aqua == "#8bba7f" and
        .defs.blue == "#80aa9e" and
        .defs.purple == "#d3869b" and
        .defs.diffRed == "#402120" and
        .defs.diffGreen == "#34381b" and
        .theme.background == "bg0" and
        .theme.text == "fg0" and
        (.theme | length) >= 50
      ' ${stachanMixTheme} >/dev/null
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
        .lsp == false and
        (.model == null) and
        (.enabled_providers == null) and
        .agent.kernel.mode == "all" and
        .agent.language.mode == "all"
      ' resolved.json >/dev/null

      mkdir project
      install -m 0600 ${self}/opencode.json project/opencode.json
      (
        cd project
        opencode debug config >../project.json
      )
      # opencode.json is the source of truth for this project's language
      # servers. Assert that every executable it names is provided, rather
      # than restating the table and asserting the file equals itself.
      missing=$(
        jq -r '
          .lsp | to_entries[]
          | select(.value.disabled != true)
          | .value.command[0]
        ' project.json | sort -u | while read -r executable; do
          command -v "$executable" >/dev/null || echo "$executable"
        done
      )
      if [ -n "$missing" ]; then
        echo "no package provides: $missing" >&2
        exit 1
      fi

      # Policy that the file alone does not explain.
      jq -e --arg home "$HOME" '
        (.lsp.oxlint.disabled == true) and
        (.lsp.typescript.initialization.tsserver.path ==
          ($home + "/.local/share/nix-typescript/lib/tsserver.js"))
      ' project.json >/dev/null
      touch "$out"
    ''
