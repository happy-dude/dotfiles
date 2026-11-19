{
  homes,
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  opencode = import ./package.nix {inherit pkgs;};
  otlpEnvironment = import ../lib/otlp-environment.nix;
  homeList = lib.attrValues homes;
  languageServerPackages = (import ../lib/language-servers.nix {inherit lib pkgs;}).packages;
  # The provider-neutral files are identical on every profile; name the one
  # copy, or fail naming the profiles that disagree.
  sharedFile = path:
    shared homes "xdg.configFile.${path}" (
      home: toString home.config.xdg.configFile.${path}.source
    );
  settings = sharedFile "opencode/opencode.json";
  cli = pkgs.writeText "opencode-cli-overlay.json" opencode.cliOverlay;
  adopter = import ./adopter.nix {inherit pkgs;};
  theme = sharedFile "opencode/themes/gruvbox-material.json";
  mixTheme = sharedFile "opencode/themes/gruvbox-material-mix-dark-medium.json";
  agentNames = lib.attrNames (import ../agents/prompts.nix {inherit lib;});
in
  assert lib.all (
    home: lib.all (package: lib.elem package home.config.home.packages) languageServerPackages
  )
  homeList;
  assert lib.all (home: lib.elem opencode home.config.home.packages) homeList;
  assert lib.all (home: !(home.config.xdg.configFile ? "opencode/cli.json")) homeList;
  assert lib.all (
    home: let
      inherit (home.config.home) activation;
    in
      lib.elem "writeBoundary" activation.checkOpenCodeCli.before
      && lib.elem "writeBoundary" activation.adoptOpenCodeCli.after
      && lib.elem "retireAgentDirectoryLinks" activation.adoptOpenCodeCli.before
      && lib.elem "linkGeneration" activation.adoptOpenCodeCli.before
  )
  homeList;
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
          adopter
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
        for variable in ${lib.escapeShellArgs otlpEnvironment}; do
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
        # Both the outgoing HM link and one the theme picker has already
        # displaced can occur on upgrade. Never seed over a regular file.
        store="$PWD/store"
        old="$store/old-generation"
        files="$store/old-home-manager-files"
        native="$XDG_CONFIG_HOME/opencode/cli.json"
        mkdir -p "$old" "$files/.config/opencode"
        ln -s "$files" "$old/home-files"
        printf '%s\n' '{"theme":{"name":"old"},"mouse":false}' >"$store/cli"
        cp "$store/cli" cli.outgoing
        ln -s "$store/cli" "$files/.config/opencode/cli.json"
        # A direct link to the same payload is not the generation's link.
        ln -s "$store/cli" "$native"
        for mode in --check ""; do
          if adopt-opencode-cli $mode "$native" "$old" "$store"; then exit 1; fi
          test "$(readlink "$native")" = "$store/cli"
        done
        rm "$native"
        ln -s "$files/.config/opencode/cli.json" "$native"
        # The deployed config path can spell its home prefix differently
        # from HOME (e.g. /home -> /var/home). Below HOME even a redirect
        # back into the correct directory must not authorize an adoption.
        ln -s "$HOME/.config" "$HOME/config-alias"
        mkdir -p "$PWD/external/opencode"
        external="$PWD/external/opencode/cli.json"
        ln -s "$files/.config/opencode/cli.json" "$external"
        for mode in --check ""; do
          if adopt-opencode-cli $mode "$HOME/config-alias/opencode/cli.json" "$old" "$store"; then exit 1; fi
          if adopt-opencode-cli $mode "$external" "$old" "$store" 2>message; then exit 1; fi
          grep -F 'outside the Home Manager home' message
          test -L "$native"
          test -L "$external"
        done
        ln -s "$PWD" alias-prefix
        configured="$PWD/alias-prefix/home/.config/opencode/cli.json"
        adopt-opencode-cli --check "$configured" "$old" "$store"
        test -L "$native"
        adopt-opencode-cli "$configured" "$old" "$store"
        rm alias-prefix "$HOME/config-alias"
        test ! -L "$native"
        test "$(stat -c %a "$native")" = 600
        cmp "$native" "$store/cli"
        printf '%s\n' '{"mouse":true,"theme":{"name":"personal"}}' >"$native"
        cp "$native" cli.before
        identity=$(stat -c '%i %y' "$native")
        adopt-opencode-cli "$native" "$old" "$store"
        cmp cli.before "$native"
        test "$identity" = "$(stat -c '%i %y' "$native")"
        # plugin list uses the CLI config service. Trigger its native atomic
        # write (migration of copy_on_select); unrelated choices must survive.
        jq '.terminal.copy_on_select = true' cli.before >"$native"
        opencode plugin list --builtin >/dev/null
        jq -e '.terminal.copy == "select" and .mouse == true and .theme.name == "personal"' "$native" >/dev/null
        cmp cli.outgoing "$store/cli"
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
            {action: "external_directory", resource: "*", effect: "ask"},
            {action: "lsp", resource: "*", effect: "allow"}
          ] and
          $info.lsp == false and
          ($info.model == null) and
          ($info.policies == null) and
          ($info.providers == null) and
          all($agents[]; $info.agents[.].mode == "all")
        ' resolved.json >/dev/null

        # Exercise the packaged permission evaluator, not merely the input
        # document. This API returns a decision without executing an edit.
        mkdir permission-project
        pushd permission-project >/dev/null
        # Keep one location alive across requests. A session ID persists in
        # SQLite, but a new standalone server need not have loaded its agents.
        service_config="$XDG_CONFIG_HOME/opencode/service.json"
        mv "$service_config" "$service_config.disabled"
        opencode service start >/dev/null
        trap 'opencode service stop' EXIT
        opencode debug agents >/dev/null
        session=$(opencode api POST /api/session -d "$(jq -nc --arg dir "$PWD" '{location:{directory:$dir}}')" | jq -er '.data.id')
        decision() {
          local payload
          payload=$(jq -nc --arg agent "$1" --arg file "$2" '{agent:$agent,action:"edit",resources:[$file]}')
          opencode api POST "/api/session/$session/permission" -d "$payload" | jq -er '.data.effect'
        }
        test "$(decision plan "$PWD/ordinary.txt")" = deny
        test "$(decision plan "$HOME/.opencode/plan/work.md")" = allow
        for agent in build ${lib.escapeShellArgs agentNames}; do
          test "$(decision "$agent" "$PWD/ordinary.txt")" = allow
        done
        opencode service stop
        trap - EXIT
        mv "$service_config.disabled" "$service_config"
        popd >/dev/null

        # Resolve only this synthetic variable in a disposable home. This
        # checks the content delivered by the wrapper rather than assuming
        # the CLI and server share the global opencode.json schema.
        printf '%s\n' '{"agents":{"probe":{"description":"{env:OPENCODE_CLI_CONFIG_CONTENT}"}}}' >"$OPENCODE_CONFIG"
        opencode debug config >overlay.json
        jq -e --slurpfile expected ${cli} '
          any(.[]; .info.agents.probe.description? == ($expected[0] | tojson))
        ' overlay.json >/dev/null
        rm "$OPENCODE_CONFIG"

        mkdir project
        install -m 0600 ${self}/opencode.json project/opencode.json
        (
          cd project
          opencode debug config >../project.json
        )
        project_path=$(realpath project/opencode.json)
        # The pin normalizes these declarations, but does not dispatch LSP
        # diagnostics from edit/write. Test discovery and PATH only, not a
        # running server. Do not let missing entries make the loop vacuous.
        jq -e --arg path "$project_path" --slurpfile declared ${self}/opencode.json '
          any(.[]; .type == "document" and .path == $path and .info.lsp == $declared[0].lsp)
        ' project.json >/dev/null
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
