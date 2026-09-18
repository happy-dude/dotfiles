{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  settings = import ./settings.nix;
  omp = import ./package.nix {inherit pkgs settings;};
  homeList = lib.attrValues homes;
  # The provider-neutral files are identical on every profile; name the one
  # copy, or fail naming the profiles that disagree.
  sharedFile = path:
    shared homes "home.file.${path}" (
      home: toString home.config.home.file.${path}.source
    );
  agentNames = ["kernel" "language"];
  agents = lib.genAttrs agentNames (name: sharedFile ".omp/agent/agents/${name}.md");
  theme = sharedFile ".omp/agent/themes/gruvbox-material.json";
  mixTheme = sharedFile ".omp/agent/themes/gruvbox-material-mix-dark-medium.json";
in
  assert lib.all (home: lib.elem omp home.config.home.packages) homeList;
    mkCheck {
      name = "dotfiles-omp-check";
      tools = [pkgs.check-jsonschema pkgs.jq omp];
      script = ''
        export HOME="$PWD/home"
        export OMP_SKIP_SETUP=1
        export OTEL_EXPORTER_OTLP_ENDPOINT=https://telemetry.invalid
        export OTEL_EXPORTER_OTLP_HEADERS=authorization=test-only
        export OTEL_RESOURCE_ATTRIBUTES=service.namespace=dotfiles-test
        mkdir -p "$HOME/.omp/agent/agents"
        ${lib.concatMapStrings (name: ''
            ln -s ${agents.${name}} "$HOME/.omp/agent/agents/${name}.md"
          '')
          agentNames}

        # The wrapper carries the settings overlay and drops the host's
        # exporter configuration before omp starts.
        wrapper=$(command -v omp)
        # The overlay must come first, so a user's own PI_CONFIG_FILES entries
        # load after it and win: makeWrapper's --prefix form writes the path
        # before the variable's previous value.
        grep -E "^PI_CONFIG_FILES='/nix/store/[^']*-omp-config[.]yml'[$]PI_CONFIG_FILES\$" "$wrapper"
        for variable in \
          OTEL_EXPORTER_OTLP_ENDPOINT \
          OTEL_EXPORTER_OTLP_TRACES_ENDPOINT \
          OTEL_EXPORTER_OTLP_LOGS_ENDPOINT \
          OTEL_EXPORTER_OTLP_METRICS_ENDPOINT \
          OTEL_EXPORTER_OTLP_HEADERS \
          OTEL_RESOURCE_ATTRIBUTES; do
          grep -F "unset $variable" "$wrapper"
        done

        # Effective settings come from the overlay alone: omp is given no
        # config.yml and must not have needed to write one.
        omp config list --json >settings.json
        test ! -e "$HOME/.omp/agent/config.yml"
        jq -e '
          .["tools.approvalMode"].value == "write" and
          .["startup.checkUpdate"].value == false and
          .["marketplace.autoUpdate"].value == "off" and
          .["dev.autoqa"].value == false and
          .["skills.customDirectories"].value == ["~/.claude/skills"] and
          .["theme.dark"].value == "gruvbox-material-mix-dark-medium"
        ' settings.json >/dev/null

        # Both themes satisfy the schema omp ships, every colour token names
        # a palette variable, and the mix variant is the base theme with the
        # same role mapping and exactly the mix palette's colours redefined.
        schema=${omp}/lib/omp/packages/coding-agent/src/modes/theme/theme-schema.json
        check-jsonschema --schemafile "$schema" ${theme} ${mixTheme}
        jq -e '.name == "gruvbox-material"' ${theme} >/dev/null
        jq -e '.name == "gruvbox-material-mix-dark-medium"' ${mixTheme} >/dev/null
        jq -e '([.colors[]] - (.vars | keys)) == []' ${theme} >/dev/null
        jq -e --slurpfile mix ${mixTheme} '
          .colors == $mix[0].colors
          and (.vars | keys) == ($mix[0].vars | keys)
          and ([.vars | to_entries[] | select(.value != $mix[0].vars[.key]) | .key]
            == ["aqua", "blue", "fg0", "fg1", "green", "orange", "red", "yellow"])
        ' ${theme} >/dev/null

        # omp requires name and description in an agent's frontmatter and
        # takes the rest of the file as the prompt; that rest must be the
        # canonical prompt body.
        ${lib.concatMapStrings (name: ''
            file="$HOME/.omp/agent/agents/${name}.md"
            test "$(sed -n 1p "$file")" = '---'
            grep -Fx 'name: ${name}' "$file"
            grep -q '^description: ".\+"$' "$file"
            test "$(sed -n 4p "$file")" = '---'
            diff -B <(sed '1,/^---$/d' ${../agents/prompts + "/${name}.md"}) \
              <(tail -n +6 "$file")
          '')
          agentNames}
      '';
    }
