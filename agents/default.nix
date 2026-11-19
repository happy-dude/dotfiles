{
  config,
  lib,
  pkgs,
  ...
}: let
  repo = "${config.home.homeDirectory}/dotfiles";
  liveLink = path: config.lib.file.mkOutOfStoreSymlink "${repo}/${path}";
  toml = pkgs.formats.toml {};
  codex = import ./codex.nix {inherit pkgs;};
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
  # The materializer owns exactly the keys generated here: MANAGED_KEYS in
  # agents/materialize_codex_profile.py lists them, and the
  # codex-profile-materializer check fails when the two differ.
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

  # The materializer command for one profile; `--check` makes its refusals
  # without writing.
  materializeProfile = options: name: ''
    ${lib.escapeShellArgs ([(lib.getExe codex.profileMaterializer)] ++ options)} \
      --old-generation "''${oldGenPath:-}" \
      ${profileFiles.${name}} \
      "$HOME/.codex/${name}.config.toml"
  '';
  prepareAgentState = phase: ''${lib.getExe codex.agentDirectoryMigration} ${phase} ${builtins.storeDir} "''${oldGenPath:-}"'';
in {
  # Claude Code registers a subagent only from a prompt carrying YAML
  # frontmatter, so link the canonical prompts individually rather than the
  # directory, which also holds the frontmatterless Kagi chat prompts.
  home.file =
    lib.mapAttrs' (
      name: _:
        lib.nameValuePair ".claude/agents/${name}.md" {
          source = liveLink "agents/prompts/${name}.md";
        }
    )
    prompts
    // lib.mapAttrs' (
      name: source:
        lib.nameValuePair ".codex/agents/${name}.toml" {inherit source;}
    )
    agentFiles;

  xdg.dataFile =
    lib.mapAttrs' (
      name: source:
        lib.nameValuePair "codex/generated-profiles/${name}.config.toml" {inherit source;}
    )
    profileFiles;

  # Check before HM follows the incoming agents' parent links. Do not
  # unlink, mkdir or chmod until *all* preflight checks have succeeded.
  home.activation.checkAgentStateDirectories = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
    ${prepareAgentState "check"}
  '';
  home.activation.secureAgentStateDirectories = lib.hm.dag.entryBetween ["linkGeneration"] ["writeBoundary"] ''
    $DRY_RUN_CMD ${prepareAgentState "secure"}
  '';

  # Adopt an outgoing store profile *before* HM cleans obsolete links. A
  # regular replacement is no longer HM's link and survives its cleanup.
  home.activation.ensureCodexProfiles = lib.hm.dag.entryBetween ["linkGeneration"] ["secureAgentStateDirectories"] (
    lib.concatMapStrings (name: ''
      $DRY_RUN_CMD ${materializeProfile [] name}
    '')
    agentNames
  );

  # Until HM is ready to install the new per-file links, leave the old
  # directory links reachable even if a profile or CLI save aborts adoption.
  home.activation.retireAgentDirectoryLinks = lib.hm.dag.entryBetween ["linkGeneration"] ["ensureCodexProfiles" "checkFilesChanged"] ''
    $DRY_RUN_CMD ${prepareAgentState "retire"}
  '';

  # The later write reruns the merge on current bytes. A refusal here,
  # including in dry-run mode, leaves the previous generation in place.
  home.activation.checkCodexProfiles = lib.hm.dag.entryBetween ["writeBoundary"] ["checkLinkTargets"] (
    lib.concatMapStrings (materializeProfile ["--check"]) agentNames
  );
}
