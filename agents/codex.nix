{pkgs}: let
  configSchemaUrl = "https://developers.openai.com/codex/config-schema.json";

  agentDirectoryMigration =
    pkgs.writers.writePython3Bin
    "migrate-codex-agent-directory"
    {}
    (builtins.readFile ./migrate_codex_agent_directory.py);

  profileMaterializer =
    pkgs.writers.writePython3Bin
    "materialize-codex-profile"
    {libraries = [pkgs.python3Packages.tomlkit];}
    (builtins.readFile ./materialize_codex_profile.py);

  agentDirectoryMigrationCheck =
    pkgs.runCommand
    "codex-agent-directory-migration-test"
    {nativeBuildInputs = [agentDirectoryMigration];}
    ''
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

      touch "$out"
    '';

  profileMaterializerCheck =
    pkgs.runCommand
    "codex-profile-materializer-test"
    {
      nativeBuildInputs = [
        profileMaterializer
        pkgs.python3
      ];
    }
    ''
      mkdir work
      printf '%s\n' \
        '#:schema ${configSchemaUrl}' \
        'developer_instructions = """' \
        'new instructions' \
        '"""' \
        'model_reasoning_effort = "medium"' \
        >work/generated.toml
      printf '%s\n' \
        'developer_instructions = "old instructions"' \
        'model_reasoning_effort = "low"' \
        'service_tier = "fast"' \
        "" \
        '[projects."/tmp/project"]' \
        'trust_level = "trusted"' \
        "" \
        '[tui.model_availability_nux]' \
        'model = 2' \
        >work/profile.toml

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
      assert profile["projects"]["/tmp/project"]["trust_level"] == "trusted"
      assert profile["tui"]["model_availability_nux"]["model"] == 2
      assert stat.S_IMODE(path.stat().st_mode) == 0o600
      assert text.startswith(
          "#:schema ${configSchemaUrl}\n"
          'developer_instructions = """\n'
      )
      assert text.count("#:schema ") == 1
      PYTHON

      before=$(sha256sum work/profile.toml)
      materialize-codex-profile work/generated.toml work/profile.toml
      after=$(sha256sum work/profile.toml)
      [[ $before == "$after" ]]

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

      touch "$out"
    '';
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
