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
      inherit (home.config.home) activation;
    in
      lib.elem "checkLinkTargets" activation.checkAgentStateDirectories.before
      && lib.elem "checkLinkTargets" activation.checkCodexProfiles.after
      && lib.elem "writeBoundary" activation.checkCodexProfiles.before
      && lib.elem "writeBoundary" activation.secureAgentStateDirectories.after
      && lib.elem "secureAgentStateDirectories" activation.ensureCodexProfiles.after
      && lib.elem "linkGeneration" activation.ensureCodexProfiles.before
      && lib.elem "ensureCodexProfiles" activation.retireAgentDirectoryLinks.after
      && lib.elem "linkGeneration" activation.retireAgentDirectoryLinks.before
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
    tools = [
      agentDirectoryMigration
      profileMaterializer
      (pkgs.python3.withPackages (ps: [ps.tomlkit (import ../lib/python {inherit pkgs;})]))
    ];
    script = ''
      mkdir -p homes
      ln -s "$PWD/homes" home-alias
      export HOME="$PWD/home-alias/user"
      store="$PWD/store"
      gen="$store/old-generation"
      files="$store/old-home-manager-files"
      mkdir -p "$gen" "$files"/{.claude,.codex} "$HOME"/{.claude,.codex} prompts
      chmod 0755 "$HOME/.claude"
      ln -s "$files" "$gen/home-files"
      printf 'tracked prompt\n' >prompts/kernel.md
      ln -s "$PWD/prompts" "$files/.claude/agents"
      ln -s "$files/.claude/agents" "$HOME/.claude/agents"
      ln -s "$PWD/missing-codex-agents" "$files/.codex/agents"
      ln -s "$files/.codex/agents" "$HOME/.codex/agents"

      # A later profile refusal cannot strand the still-active generation.
      migrate-codex-agent-directory check "$store" "$gen"
      printf 'developer_instructions = "new"\n' >"$store/template"
      printf 'broken = "\n' >"$HOME/.codex/kernel.config.toml"
      if materialize-codex-profile --check --old-generation "$gen" "$store/template" "$HOME/.codex/kernel.config.toml"; then
        exit 1
      fi
      test -L "$HOME/.claude/agents"
      test -L "$HOME/.codex/agents"
      test "$(cat "$HOME/.claude/agents/kernel.md")" = 'tracked prompt'
      test "$(stat -c %a "$HOME/.claude")" = 755
      test ! -e "$HOME/.omp"

      # A direct link to the very same repository is not proof of ownership.
      ln -sfnT "$PWD/prompts" "$HOME/.claude/agents"
      for phase in check secure retire; do
        if migrate-codex-agent-directory "$phase" "$store" "$gen"; then exit 1; fi
        test -L "$HOME/.codex/agents"
        test ! -e "$HOME/.omp"
      done
      ln -sfnT "$files/.claude/agents" "$HOME/.claude/agents"
      migrate-codex-agent-directory secure "$store" "$gen"
      test -L "$HOME/.claude/agents"
      test -L "$HOME/.codex/agents"
      test "$(cat prompts/kernel.md)" = 'tracked prompt'
      for dir in .claude .codex .omp; do test "$(stat -c %a "$HOME/$dir")" = 700; done

      # A client save after profile staging aborts the write phase. Both
      # agent directory links must still be readable as links at that point.
      python3 - ${./materialize_codex_profile.py} "$store/template" <<'PYTHON'
      import runpy
      import sys
      from pathlib import Path
      from unittest.mock import patch

      profile = Path.home() / ".codex/kernel.config.toml"
      profile.write_text('service_tier = "old"\n')
      materialize = runpy.run_path(sys.argv[1])["materialize"]
      def client_save(_fd):
          saved = profile.with_suffix(".new")
          saved.write_text('service_tier = "saved"\n')
          saved.replace(profile)
      with patch("os.fsync", side_effect=client_save):
          try:
              materialize(Path(sys.argv[2]), profile, check_only=False, old_generation=Path())
          except SystemExit:
              pass
          else:
              raise AssertionError("overwrote Codex's concurrent save")
      assert profile.read_text() == 'service_tier = "saved"\n'
      PYTHON
      test -L "$HOME/.claude/agents"
      test -L "$HOME/.codex/agents"
      test "$(cat "$HOME/.claude/agents/kernel.md")" = 'tracked prompt'

      # The supported immutable link carries runtime keys as well as the
      # old generated keys. An identical terminal payload via a different
      # generation/slot, mutable hop, or missing leaf is not sufficient.
      profile="$HOME/.codex/kernel.config.toml"
      entry="$files/.codex/kernel.config.toml"
      payload="$store/old-profile.toml"
      printf 'developer_instructions = "old"\nservice_tier = "fast"\n' >"$payload"
      ln -s "$payload" "$entry"
      ln -s "$payload" "$files/.codex/other.config.toml"
      mkdir -p "$store/other-home-manager-files/.codex"
      ln -s "$payload" "$store/other-home-manager-files/.codex/kernel.config.toml"
      for link in "$payload" "$files/.codex/other.config.toml" "$store/other-home-manager-files/.codex/kernel.config.toml"; do
        ln -sfnT "$link" "$profile"
        for mode in --check ""; do
          if materialize-codex-profile $mode --old-generation "$gen" "$store/template" "$profile"; then exit 1; fi
          test "$(readlink "$profile")" = "$link"
        done
      done
      ln -sfnT "$entry" "$profile"
      ln -s "$payload" mutable-link
      for link in "$PWD/mutable-link" "$store/missing"; do
        ln -sfnT "$link" "$entry"
        if materialize-codex-profile --check --old-generation "$gen" "$store/template" "$profile"; then exit 1; fi
        test -L "$profile"
      done
      ln -sfnT "$payload" "$entry"
      materialize-codex-profile --check --old-generation "$gen" "$store/template" "$profile"
      test -L "$profile"
      materialize-codex-profile --old-generation "$gen" "$store/template" "$profile"
      test ! -L "$profile"
      test "$(stat -c %a "$profile")" = 600
      grep -Fx 'developer_instructions = "new"' "$profile"
      grep -Fx 'service_tier = "fast"' "$profile"
      grep -Fx 'developer_instructions = "old"' "$payload"
      migrate-codex-agent-directory retire "$store" "$gen"
      test ! -L "$HOME/.claude/agents"
      test ! -L "$HOME/.codex/agents"
      test "$(cat prompts/kernel.md)" = 'tracked prompt'
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
        echo "Agent validation must precede writeBoundary, adoption must precede linkGeneration" >&2
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
