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
      jq -e --arg home "$HOME" '
        .lsp.bash.command == ["bash-language-server", "start"] and
        .lsp.clangd.command == ["clangd", "--background-index", "--clang-tidy"] and
        .lsp["clojure-lsp"].command == ["clojure-lsp", "listen"] and
        .lsp.eslint.command == ["vscode-eslint-language-server", "--stdio"] and
        .lsp["fennel-ls"].command == ["fennel-ls", "--server"] and
        .lsp["fish-lsp"].command == ["fish-lsp", "start"] and
        .lsp.gopls.command == ["gopls"] and
        .lsp["haskell-language-server"].command ==
          ["haskell-language-server-wrapper", "--lsp"] and
        .lsp["json-ls"].command == ["vscode-json-language-server", "--stdio"] and
        .lsp["kotlin-ls"].command == ["kotlin-language-server"] and
        .lsp["lua-ls"].command == ["lua-language-server"] and
        .lsp.marksman.command == ["marksman", "server"] and
        .lsp.nixd.command == ["nixd"] and
        .lsp.oxlint.disabled == true and
        .lsp.perlnavigator.command == ["perlnavigator", "--stdio"] and
        .lsp.ruff.command == ["ruff", "server"] and
        .lsp.rust.command == ["rust-analyzer"] and
        .lsp.terraform.command == ["terraform-ls", "serve"] and
        .lsp.texlab.command == ["texlab"] and
        .lsp.tinymist.command == ["tinymist", "lsp"] and
        .lsp.typescript.command == ["typescript-language-server", "--stdio"] and
        .lsp.typescript.initialization.tsserver.path ==
          ($home + "/.local/share/nix-typescript/lib/tsserver.js") and
        .lsp["vim-ls"].command == ["vim-language-server", "--stdio"] and
        .lsp["yaml-ls"].command == ["yaml-language-server", "--stdio"] and
        .lsp.zls.command == ["zls"] and
        .lsp.zuban.command == ["zuban", "server"]
      ' project.json >/dev/null
      touch "$out"
    ''
