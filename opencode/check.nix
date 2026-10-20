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
  languageServerPackages = (import ../lib/language-servers.nix {inherit lib pkgs;}).packages;
  # The provider-neutral files are identical on every profile; name the one
  # copy, or fail naming the profiles that disagree.
  sharedFile = path:
    shared homes "xdg.configFile.${path}" (
      home: toString home.config.xdg.configFile.${path}.source
    );
  settings = sharedFile "opencode/opencode.json";
  cli = sharedFile "opencode/cli.json";
  theme = sharedFile "opencode/themes/gruvbox-material.json";
  mixTheme = sharedFile "opencode/themes/gruvbox-material-mix-dark-medium.json";
  agentNames = lib.attrNames (import ../agents/prompts.nix {inherit lib;});
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
  # systemd user services read environment.d, not the shell session
  # variables, so both values must reach them by that route too.
  assert lib.all (
    home:
      home.config.systemd.user.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD
      == "true"
      && home.config.systemd.user.sessionVariables.OPENCODE_CONFIG
      == home.config.home.sessionVariables.OPENCODE_CONFIG
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
        # Disable the persistent background service so each CLI invocation in
        # the check uses an isolated standalone server.
        printf '%s\n' '{"disabled":true}' >"$XDG_CONFIG_HOME/opencode/service.json"
        for variable in \
          OTEL_EXPORTER_OTLP_ENDPOINT \
          OTEL_EXPORTER_OTLP_TRACES_ENDPOINT \
          OTEL_EXPORTER_OTLP_LOGS_ENDPOINT \
          OTEL_EXPORTER_OTLP_METRICS_ENDPOINT \
          OTEL_EXPORTER_OTLP_HEADERS \
          OTEL_RESOURCE_ATTRIBUTES; do
          grep -Fx "unset $variable" "$(command -v opencode)"
        done
        check-jsonschema \
          --schemafile ${opencode}/share/cli.json \
          ${cli}
        check-jsonschema \
          --schemafile ${opencode}/share/config.json \
          ${settings} ${self}/opencode.json
        check-jsonschema \
          --schemafile ${opencode}/share/theme.json \
          ${theme} ${mixTheme}
        jq -e '
          .theme.name == "gruvbox-material-mix-dark-medium"
        ' ${cli} >/dev/null
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
        jq -e --arg path "$XDG_CONFIG_HOME/opencode/opencode.json" \
          --argjson agents ${lib.escapeShellArg (builtins.toJSON agentNames)} '
          (.[] | select(.type == "document" and .path == $path) | .info) as $info |
          ([.[] | select(.type == "document")] | length) == 1 and
          $info.share == "disabled" and
          $info.update == "disable" and
          $info.permissions == [
            {action: "shell", resource: "*", effect: "ask"},
            {action: "edit", resource: "*", effect: "allow"},
            {action: "external_directory", resource: "*", effect: "ask"},
            {action: "lsp", resource: "*", effect: "allow"}
          ] and
          $info.lsp == false and
          ($info.model == null) and
          ($info.policies == null) and
          ($info.providers == null) and
          all($agents[]; $info.agents[.].mode == "all")
        ' resolved.json >/dev/null

        mkdir project
        install -m 0600 ${self}/opencode.json project/opencode.json
        (
          cd project
          opencode debug config >../project.json
        )
        project_path=$(realpath project/opencode.json)
        # opencode.json is the source of truth for this project's language
        # servers. Assert that every executable it names is provided, rather
        # than restating the table and asserting the file equals itself.
        missing=$(
          jq -r --arg path "$project_path" '
            .[]
            | select(.type == "document" and .path == $path)
            | .info.lsp
            | to_entries[]
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

        # Keep Oxlint off in this repo so its diagnostics do not overlap with ESLint.
        jq -e --arg path "$project_path" '
          (.[] | select(.type == "document" and .path == $path) | .info.lsp.oxlint.disabled) == true
        ' project.json >/dev/null
      '';
    }
