{
  homes ? {},
  pkgs,
}: let
  inherit (pkgs) lib;
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  mkPythonScript = import ../lib/python/mkScript.nix {inherit pkgs;};
  generatedProfiles =
    if homes == {}
    then []
    else
      shared homes "the Codex profile templates" (
        home:
          lib.mapAttrsToList (
            _: entry: entry.source
          ) (
            lib.filterAttrs (
              name: _: lib.hasPrefix "codex/generated-profiles/" name
            )
            home.config.xdg.dataFile
          )
      );
  configSchemaUrl = "https://developers.openai.com/codex/config-schema.json";
  # The read-only profile check has to run once link targets are known to be
  # safe and before anything is written.
  profileCheckPrecedesWrites = lib.all (
    home: let
      entry = home.config.home.activation.checkCodexProfiles;
    in
      lib.elem "checkLinkTargets" entry.after && lib.elem "writeBoundary" entry.before
  ) (lib.attrValues homes);

  agentDirectoryMigration = mkPythonScript {
    name = "migrate-codex-agent-directory";
    source = ./migrate_codex_agent_directory.py;
  };

  profileMaterializer = mkPythonScript {
    name = "materialize-codex-profile";
    source = ./materialize_codex_profile.py;
    libraries = [pkgs.python3Packages.tomlkit];
  };

  agentDirectoryMigrationCheck = mkCheck {
    name = "codex-agent-directory-migration-test";
    tools = [agentDirectoryMigration];
    script = ''
      mkdir -p \
        work/legacy \
        work/store/old-home-manager-files/.codex/agents
      touch \
        work/store/codex-agent-kernel.toml \
        work/store/codex-agent-language.toml \
        work/store/old-codex-agent-kernel.toml
      ln -s \
        "$PWD/work/store/old-codex-agent-kernel.toml" \
        work/store/old-home-manager-files/.codex/agents/kernel.toml
      touch work/legacy/kagi-kernel.toml
      ln -s \
        "$PWD/work/store/old-home-manager-files/.codex/agents/kernel.toml" \
        work/legacy/kernel.toml
      ln -s \
        "$PWD/work/store/codex-agent-language.toml" \
        work/legacy/language.toml
      ln -s "$PWD/work/legacy" work/agents

      migrate-codex-agent-directory \
        "$PWD/work/legacy" \
        "$PWD/work/agents" \
        "kernel=$PWD/work/store/codex-agent-kernel.toml" \
        "language=$PWD/work/store/codex-agent-language.toml"

      [[ ! -e work/agents && ! -L work/agents ]]
      [[ ! -e work/legacy/kernel.toml && ! -L work/legacy/kernel.toml ]]
      [[ ! -e work/legacy/language.toml && ! -L work/legacy/language.toml ]]
      [[ -f work/legacy/kagi-kernel.toml ]]


      # Former Claude whole-directory links (dangling claude/.claude/agents or
      # live agents/prompts with tracked regular files) are unlinked without
      # touching the tracked prompt files.
      mkdir -p \
        work/prompts \
        work/store/old-claude-home-manager-files/.claude
      touch work/prompts/kernel.md work/prompts/language.md
      ln -s \
        "$PWD/work/deleted-claude-agents" \
        work/store/old-claude-home-manager-files/.claude/agents
      ln -s \
        "$PWD/work/store/old-claude-home-manager-files/.claude/agents" \
        work/claude-agents
      migrate-codex-agent-directory \
        "$PWD/work/deleted-claude-agents:$PWD/work/prompts" \
        "$PWD/work/claude-agents"
      [[ ! -e work/claude-agents && ! -L work/claude-agents ]]

      ln -s "$PWD/work/prompts" work/claude-agents
      migrate-codex-agent-directory \
        "$PWD/work/deleted-claude-agents:$PWD/work/prompts" \
        "$PWD/work/claude-agents"
      [[ ! -e work/claude-agents && ! -L work/claude-agents ]]
      [[ -f work/prompts/kernel.md && -f work/prompts/language.md ]]
      mkdir work/agents
      migrate-codex-agent-directory \
        "$PWD/work/legacy" \
        "$PWD/work/agents" \
        "kernel=$PWD/work/store/codex-agent-kernel.toml" \
        "language=$PWD/work/store/codex-agent-language.toml"

      ln -s "$PWD/work/store" work/unrelated
      if migrate-codex-agent-directory \
        "$PWD/work/legacy" \
        "$PWD/work/unrelated" \
        "kernel=$PWD/work/store/codex-agent-kernel.toml"; then
        echo "Migration accepted an unrelated agent directory" >&2
        exit 1
      fi

      touch work/legacy/kernel.toml
      if migrate-codex-agent-directory \
        "$PWD/work/legacy" \
        "$PWD/work/agents" \
        "kernel=$PWD/work/store/codex-agent-kernel.toml"; then
        echo "Migration accepted an unmanaged legacy agent file" >&2
        exit 1
      fi
    '';
  };

  profileMaterializerCheck = mkCheck {
    name = "codex-profile-materializer-test";
    tools = [
      profileMaterializer
      pkgs.python3
    ];
    script = ''
      ${lib.optionalString (!profileCheckPrecedesWrites) ''
        echo "checkCodexProfiles must run after checkLinkTargets and before writeBoundary" >&2
        exit 1
      ''}
      mkdir work
      printf '%s\n' \
        '#:schema ${configSchemaUrl}' \
        'developer_instructions = """' \
        'new instructions' \
        '"""' \
        'model_reasoning_effort = "medium"' \
        >work/generated.toml
      # A dotted key parses as a table, so it precedes the scalars below.
      printf '%s\n' \
        'notice.seen = true' \
        'developer_instructions = "old instructions"' \
        'model_reasoning_effort = "low"' \
        'service_tier = "fast"' \
        'hide_agent_reasoning = true' \
        'show_raw_agent_reasoning = false' \
        "" \
        '[projects."/tmp/project"]' \
        'trust_level = "trusted"' \
        "" \
        '[tui.model_availability_nux]' \
        'model = 2' \
        >work/profile.toml

      # The check mode, which activation runs before any link is switched,
      # accepts what a real run accepts and writes nothing: a profile that
      # needs refreshing keeps its bytes, and a missing one is not created.
      cp work/profile.toml work/profile.before
      identity=$(stat -c '%i %y' work/profile.toml)
      materialize-codex-profile --check work/generated.toml work/profile.toml
      cmp work/profile.before work/profile.toml
      [[ $identity == "$(stat -c '%i %y' work/profile.toml)" ]]
      materialize-codex-profile --check work/generated.toml work/absent/profile.toml
      test ! -e work/absent

      # refuses DESCRIPTION TEMPLATE TARGET: the check mode and a real run
      # both reject the pair.
      refuses() {
        local mode
        for mode in --check ""; do
          if materialize-codex-profile $mode "$2" "$3"; then
            echo "Materializer accepted $1''${mode:+ in check mode}" >&2
            exit 1
          fi
        done
      }

      materialize-codex-profile work/generated.toml work/profile.toml
      python3 - work/profile.toml <<'PYTHON'
      import stat
      import sys
      import tomllib
      from pathlib import Path

      path = Path(sys.argv[1])
      text = path.read_text(encoding="utf-8")
      profile = tomllib.loads(text)
      assert profile["developer_instructions"] == "new instructions\n"
      assert profile["model_reasoning_effort"] == "medium"
      assert profile["service_tier"] == "fast"
      assert profile["hide_agent_reasoning"] is True
      assert profile["show_raw_agent_reasoning"] is False
      assert profile["projects"]["/tmp/project"]["trust_level"] == "trusted"
      assert profile["tui"]["model_availability_nux"]["model"] == 2
      assert profile["notice"]["seen"] is True
      assert stat.S_IMODE(path.stat().st_mode) == 0o600
      assert text.startswith(
          "#:schema ${configSchemaUrl}\n"
          'developer_instructions = """\n'
      )
      assert text.count("#:schema ") == 1
      PYTHON

      # A current profile is left in place: same bytes, same inode.
      before=$(sha256sum work/profile.toml)
      inode=$(stat -c %i work/profile.toml)
      materialize-codex-profile work/generated.toml work/profile.toml
      after=$(sha256sum work/profile.toml)
      [[ $before == "$after" ]]
      [[ $inode == "$(stat -c %i work/profile.toml)" ]]

      # Current content with the wrong mode is still rewritten at 0600.
      chmod 0644 work/profile.toml
      materialize-codex-profile work/generated.toml work/profile.toml
      [[ $(stat -c %a work/profile.toml) == 600 ]]
      [[ $before == "$(sha256sum work/profile.toml)" ]]

      # A changed template updates a profile that still holds the old
      # managed keys.
      printf '%s\n' \
        '#:schema ${configSchemaUrl}' \
        'developer_instructions = """' \
        'language instructions' \
        '"""' \
        >work/generated.toml
      materialize-codex-profile work/generated.toml work/profile.toml
      python3 - work/profile.toml <<'PYTHON'
      import sys
      import tomllib
      from pathlib import Path

      path = Path(sys.argv[1])
      text = path.read_text(encoding="utf-8")
      profile = tomllib.loads(text)
      assert profile["developer_instructions"] == "language instructions\n"
      assert "model_reasoning_effort" not in profile
      assert profile["service_tier"] == "fast"
      assert profile["projects"]["/tmp/project"]["trust_level"] == "trusted"
      assert profile["tui"]["model_availability_nux"]["model"] == 2
      assert text.startswith(
          "#:schema ${configSchemaUrl}\n"
          'developer_instructions = """\n'
      )
      PYTHON


      ${lib.optionalString (generatedProfiles != []) ''
        for template in ${lib.escapeShellArgs generatedProfiles}; do
          materialize-codex-profile "$template" work/real-profile.toml
        done

        # MANAGED_KEYS must be exactly the keys the templates carry between
        # them. A template key it lacks fails activation, as the loop above
        # shows; a key it lists that no template sets would be deleted from
        # every profile, whoever wrote it.
        python3 - ${./materialize_codex_profile.py} ${lib.escapeShellArgs generatedProfiles} <<'PYTHON'
        import ast
        import sys
        import tomllib
        from pathlib import Path

        source = ast.parse(Path(sys.argv[1]).read_text(encoding="utf-8"))
        managed = {
            key
            for node in source.body
            if isinstance(node, ast.Assign)
            and [target.id for target in node.targets] == ["MANAGED_KEYS"]
            for key in ast.literal_eval(node.value)
        }
        assert managed, "MANAGED_KEYS not found in the materializer"
        generated = set()
        for template in sys.argv[2:]:
            generated |= tomllib.loads(Path(template).read_text(encoding="utf-8")).keys()
        if managed != generated:
            raise SystemExit(
                "MANAGED_KEYS and the generated Codex profiles disagree: "
                f"only in MANAGED_KEYS {sorted(managed - generated)}, "
                f"only in the profiles {sorted(generated - managed)}"
            )
        PYTHON
      ''}

      # A template the materializer does not fully own is refused, and the
      # profile keeps its content.
      cp work/profile.toml work/profile.kept
      printf '%s\n' \
        'developer_instructions = "instructions"' \
        'model = "unmanaged"' \
        >work/unmanaged.toml
      refuses "a template with an unmanaged key" work/unmanaged.toml work/profile.toml
      printf '%s\n' 'model_reasoning_effort = "medium"' >work/incomplete.toml
      refuses "a template without instructions" work/incomplete.toml work/profile.toml
      cmp work/profile.kept work/profile.toml

      ln -s "$PWD/work" work/linked-dir
      refuses "a symlinked parent directory" \
        work/generated.toml work/linked-dir/profile.toml

      # Neither a valid nor a dangling link grants ownership of its target.
      cp work/profile.toml work/external.toml
      ln -s "$PWD/work/external.toml" work/linked.toml
      refuses "a symlinked profile" work/generated.toml work/linked.toml
      test -L work/linked.toml
      cmp work/profile.toml work/external.toml
      ln -s "$PWD/work/missing.toml" work/dangling.toml
      refuses "a dangling profile link" work/generated.toml work/dangling.toml
      test -L work/dangling.toml
      test ! -e work/missing.toml

      # Failed parsing must not discard runtime-owned keys.
      printf 'service_tier = "unfinished\n' >work/invalid.toml
      cp work/invalid.toml work/invalid.before
      refuses "an unparsable profile" work/generated.toml work/invalid.toml
      cmp work/invalid.before work/invalid.toml
    '';
  };
in {
  inherit
    agentDirectoryMigration
    configSchemaUrl
    profileMaterializer
    ;

  checks = {
    agentDirectoryMigration = agentDirectoryMigrationCheck;
    profileMaterializer = profileMaterializerCheck;
  };
}
