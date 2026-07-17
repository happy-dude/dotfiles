{
  codex,
  config,
  lib,
  pkgs,
  ...
}: let
  repo = "${config.home.homeDirectory}/dotfiles";
  liveLink = path: config.lib.file.mkOutOfStoreSymlink "${repo}/${path}";
  toml = pkgs.formats.toml {};
  prompts = import ./prompts.nix {inherit lib;};
  withConfigSchema = name: source:
    pkgs.runCommand "codex-profile-${name}-with-schema.toml" {} ''
      {
        printf '%s\n' '#:schema ${codex.configSchemaUrl}'
        cat ${source}
      } >"$out"
    '';
  agentNames = builtins.attrNames prompts;
  agentFiles =
    lib.mapAttrs (
      name: prompt:
        toml.generate "codex-agent-${name}.toml" {
          inherit (prompt) name description;
          developer_instructions = prompt.body + "\n";
        }
    )
    prompts;
  profileFiles =
    lib.mapAttrs (
      name: prompt:
        withConfigSchema name (
          toml.generate "codex-profile-${name}.toml" (
            {developer_instructions = prompt.body + "\n";}
            // lib.optionalAttrs (name == "kernel") {
              model_reasoning_effort = "medium";
            }
          )
        )
    )
    prompts;

  materializeProfile = name: profile: ''
    ${lib.getExe codex.profileMaterializer} \
      ${profile} \
      "$HOME/.codex/${name}.config.toml"
  '';
  legacyAgentDirectory = "${repo}/agents/generated/codex-agents";
in {
  home.file =
    {
      ".claude/agents".source = liveLink "agents/prompts";
      ".codex/agents/kagi-kernel.toml".source =
        liveLink "agents/generated/codex-agents/kagi-kernel.toml";
      ".codex/agents/kagi-language.toml".source =
        liveLink "agents/generated/codex-agents/kagi-language.toml";
    }
    // lib.mapAttrs' (
      name: source:
        lib.nameValuePair ".codex/agents/${name}.toml" {inherit source;}
    )
    agentFiles;

  xdg.dataFile =
    lib.mapAttrs' (
      name: source:
        lib.nameValuePair "codex/generated-profiles/${name}.config.toml" {
          inherit source;
          onChange = materializeProfile name source;
        }
    )
    profileFiles;

  home.activation.migrateCodexAgentDirectory = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
    $DRY_RUN_CMD ${lib.getExe codex.agentDirectoryMigration} \
      ${lib.escapeShellArg legacyAgentDirectory} \
      "$HOME/.codex/agents" \
      ${lib.escapeShellArg "kernel=${agentFiles.kernel}"} \
      ${lib.escapeShellArg "language=${agentFiles.language}"}
  '';

  home.activation.secureAgentStateDirectories = lib.hm.dag.entryAfter ["linkGeneration"] ''
    for directory in "$HOME/.claude" "$HOME/.codex"; do
      if [[ -L $directory || (-e $directory && ! -d $directory) ]]; then
        echo "Refusing malformed agent state directory: $directory" >&2
        exit 1
      fi
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/mkdir -p -- "$directory"
      $DRY_RUN_CMD ${pkgs.coreutils}/bin/chmod 0700 -- "$directory"
    done
  '';

  home.activation.ensureCodexProfiles = lib.hm.dag.entryAfter ["onFilesChange"] (
    lib.concatMapStrings (name: ''
      if [[ ! -e "$HOME/.codex/${name}.config.toml" ]]; then
        $DRY_RUN_CMD ${materializeProfile name profileFiles.${name}}
      fi
    '')
    agentNames
  );
}
