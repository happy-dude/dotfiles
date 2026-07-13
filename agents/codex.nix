{pkgs}: let
  configSchemaUrl = "https://developers.openai.com/codex/config-schema.json";

  agentDirectoryMigration = pkgs.writeShellApplication {
    name = "migrate-codex-agent-directory";
    runtimeInputs = [pkgs.coreutils];
    text = ''
      if (($# < 3)); then
        echo "usage: migrate-codex-agent-directory LEGACY TARGET NAME=SOURCE..." >&2
        exit 2
      fi

      legacy=$1
      target=$2
      shift 2
      remove_target=false
      legacy_links=()

      if [[ -L $target ]]; then
        actual=$(readlink -m -- "$target")
        expected=$(readlink -m -- "$legacy")
        if [[ $actual != "$expected" ]]; then
          echo "Refusing unrelated Codex agent directory link: $target" >&2
          exit 1
        fi
        remove_target=true
      elif [[ -e $target && ! -d $target ]]; then
        echo "Refusing non-directory Codex agent path: $target" >&2
        exit 1
      fi

      for mapping in "$@"; do
        name=''${mapping%%=*}
        source=''${mapping#*=}
        if [[ -z $name || $name == "$mapping" || $name == */* ]]; then
          echo "Invalid Codex agent migration mapping: $mapping" >&2
          exit 2
        fi

        legacy_path="$legacy/$name.toml"
        if [[ -L $legacy_path ]]; then
          actual=$(readlink -m -- "$legacy_path")
          expected=$(readlink -m -- "$source")
          if [[ $actual != "$expected" ]]; then
            link_target=$(readlink -- "$legacy_path")
            suffix="/.codex/agents/$name.toml"
            home_files=''${link_target%"$suffix"}
            store_dir=$(dirname -- "$source")
            if [[ $remove_target != true ||
              $home_files == "$link_target" ||
              $(dirname -- "$home_files") != "$store_dir" ||
              $(basename -- "$home_files") != *-home-manager-files ]]; then
              echo "Refusing unrelated legacy Codex agent link: $legacy_path" >&2
              exit 1
            fi
          fi
          legacy_links+=("$legacy_path")
        elif [[ -e $legacy_path ]]; then
          echo "Refusing unmanaged legacy Codex agent file: $legacy_path" >&2
          exit 1
        fi
      done

      for legacy_path in "''${legacy_links[@]}"; do
        rm -f -- "$legacy_path"
      done
      if [[ $remove_target == true ]]; then
        rm -f -- "$target"
      fi
    '';
  };

  profileMaterializer =
    pkgs.writers.writePython3Bin
    "materialize-codex-profile"
    {libraries = [pkgs.python3Packages.tomlkit];}
    ''
      import os
      import sys
      import tempfile
      from pathlib import Path

      import tomlkit
      from tomlkit.items import AoT, Table

      MANAGED_KEYS = (
          "developer_instructions",
          "model_reasoning_effort",
      )
      SCHEMA_DIRECTIVE = (
          "#:schema "
          "${configSchemaUrl}"
      )


      def load_document(path: Path, description: str):
          try:
              return tomlkit.parse(path.read_text(encoding="utf-8"))
          except (OSError, tomlkit.exceptions.ParseError) as error:
              message = f"Unable to read {description} {path}: {error}"
              raise SystemExit(message) from error


      def materialize(source: Path, target: Path) -> None:
          if not source.is_file():
              message = (
                  "Generated Codex profile is not a regular file: "
                  f"{source}"
              )
              raise SystemExit(message)

          generated = load_document(source, "generated Codex profile")
          unexpected = set(generated) - set(MANAGED_KEYS)
          if unexpected:
              names = ", ".join(sorted(unexpected))
              message = f"Generated Codex profile has unmanaged keys: {names}"
              raise SystemExit(message)
          if "developer_instructions" not in generated:
              raise SystemExit(
                  "Generated Codex profile lacks developer_instructions"
              )

          if target.is_symlink():
              raise SystemExit(f"Refusing symlinked Codex profile: {target}")
          if target.exists() and not target.is_file():
              raise SystemExit(f"Refusing non-regular Codex profile: {target}")

          if target.exists():
              runtime = load_document(target, "existing Codex profile")
          else:
              runtime = tomlkit.document()

          merged = tomlkit.document()
          for key in MANAGED_KEYS:
              if key in generated:
                  merged.add(key, generated.item(key))
          runtime_items = [
              (key, item, item.unwrap())
              for key, item in runtime.items()
              if key not in MANAGED_KEYS
          ]
          for key, item, value in runtime_items:
              if not isinstance(item, (Table, AoT)):
                  merged[key] = value
          for key, item, value in runtime_items:
              if isinstance(item, (Table, AoT)):
                  merged[key] = value

          target.parent.mkdir(parents=True, exist_ok=True)
          descriptor, temporary_name = tempfile.mkstemp(
              dir=target.parent,
              prefix=f".{target.name}.",
          )
          temporary = Path(temporary_name)
          try:
              os.fchmod(descriptor, 0o600)
              with os.fdopen(
                  descriptor,
                  "w",
                  encoding="utf-8",
              ) as output:
                  output.write(SCHEMA_DIRECTIVE + "\n")
                  output.write(tomlkit.dumps(merged))
                  output.flush()
                  os.fsync(output.fileno())
              os.replace(temporary, target)
              directory = os.open(
                  target.parent,
                  os.O_RDONLY | os.O_DIRECTORY,
              )
              try:
                  os.fsync(directory)
              finally:
                  os.close(directory)
          finally:
              temporary.unlink(missing_ok=True)


      if len(sys.argv) != 3:
          raise SystemExit("usage: materialize-codex-profile SOURCE TARGET")
      materialize(Path(sys.argv[1]), Path(sys.argv[2]))
    '';

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
