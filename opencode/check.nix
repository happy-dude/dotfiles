{
  homes,
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  opencode = import ./package.nix {inherit pkgs;};
  homeList = lib.attrValues homes;
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
    vim-language-server
    vscode-langservers-extracted
    yaml-language-server
    zls
    zuban
  ];
  # The provider-neutral files are identical on every profile; name the one
  # copy, or fail naming the profiles that disagree.
  sharedFile = path:
    shared homes "xdg.configFile.${path}" (
      home: toString home.config.xdg.configFile.${path}.source
    );
  settings = sharedFile "opencode/opencode.json";
  tui = sharedFile "opencode/tui.json";
  theme = sharedFile "opencode/themes/gruvbox-material.json";
  mixTheme = sharedFile "opencode/themes/gruvbox-material-mix-dark-medium.json";
in
  assert lib.all (
    home: lib.all (package: lib.elem package home.config.home.packages) languageServerPackages
  )
  homeList;
  assert lib.all (home: lib.elem opencode home.config.home.packages) homeList;
  assert lib.all (
    home: home.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true"
  )
  homeList;
  assert lib.all (
    home:
      home.config.home.sessionVariables.OPENCODE_CONFIG
      == "${home.config.xdg.configHome}/opencode/local.json"
  )
  homeList;
    mkCheck {
      name = "dotfiles-opencode-check";
      tools =
        [
          pkgs.check-jsonschema
          pkgs.jq
          opencode
        ]
        ++ languageServerPackages;
      script = ''
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
        tui_schema=${opencode}/share/tui.json
        [ -e "$tui_schema" ] || tui_schema=${opencode}/share/opencode/tui.json
        check-jsonschema \
          --schemafile "$tui_schema" \
          ${tui}
        check-jsonschema \
          --schemafile ${pkgs.opencode.src}/packages/web/public/theme.json \
          ${theme}
        check-jsonschema \
          --schemafile ${pkgs.opencode.src}/packages/web/public/theme.json \
          ${mixTheme}
        jq -e '
          .theme == "gruvbox-material-mix-dark-medium"
        ' ${tui} >/dev/null
        # The mix variant is the base theme with a brighter palette: same
        # role assignments, same palette names, and exactly these colors
        # redefined. Nix builds both from one attribute set; assert that
        # relationship.
        jq -e --slurpfile mix ${mixTheme} '
          .theme == $mix[0].theme
          and (.defs | keys) == ($mix[0].defs | keys)
          and ([.defs | to_entries[] | select(.value != $mix[0].defs[.key]) | .key]
            == ["aqua", "blue", "fg0", "fg1", "green", "orange", "red", "yellow"])
          and (.theme | length) >= 50
        ' ${theme} >/dev/null
        install -m 0600 ${settings} \
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
        jq -e '.lsp.oxlint.disabled == true' project.json >/dev/null
      '';
    }
