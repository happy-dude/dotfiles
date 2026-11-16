{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  zedSettingsMaterializer = import ./materializer.nix {inherit pkgs;};
  hostSettings = import ./settings.nix {inherit lib pkgs;};
  settingsFile = (pkgs.formats.json {}).generate "zed-settings-check.json" hostSettings;
  nixglEnvironment = import ../lib/nixgl-environment.nix;
  nixglEnvironmentFile = pkgs.writeText "nixgl-environment.json" (
    builtins.toJSON nixglEnvironment
  );
  # The wrapper scripts as the modules reach them: config.lib.nixGL.wrap runs
  # nixGLIntel for the "mesa" wrapper, and zed/default.nix names
  # nixVulkanIntel itself.
  nixglWrapper = name:
    shared homes "the ${name} wrapper" (
      home:
        lib.getExe
        home.config.targets.genericLinux.nixGL.packages.${pkgs.stdenv.hostPlatform.system}.${name}
    );
  # The read-only settings check has to run once link targets are known to be
  # safe and before anything is written.
  settingsCheckPrecedesWrites = lib.all (
    home: let
      entry = home.config.home.activation.checkZedSettings;
    in
      lib.elem "checkLinkTargets" entry.after && lib.elem "writeBoundary" entry.before
  ) (lib.attrValues homes);
in {
  # lib/nixgl-environment.nix must name every variable the pinned wrappers
  # export, or a nixGL update could add one that reaches host children. It may
  # name more, so this is a subset test.
  nixgl-environment = mkCheck {
    name = "nixgl-environment-test";
    script = ''
      status=0
      # check_wrapper SCRIPT NAME...: every variable SCRIPT exports is a NAME.
      check_wrapper() {
        local wrapper=$1 exported name
        shift
        exported=$(sed -nE 's/^[[:space:]]*export[[:space:]]+([A-Za-z_][A-Za-z0-9_]*)=.*/\1/p' "$wrapper")
        # Fail on an export the pattern cannot read rather than skip it.
        if [[ -z $exported || $(wc -l <<<"$exported") != "$(grep -cw export "$wrapper")" ]]; then
          echo "Unable to read the exports of $wrapper" >&2
          status=1
          return
        fi
        for name in $exported; do
          if [[ " $* " != *" $name "* ]]; then
            echo "$wrapper exports $name, which lib/nixgl-environment.nix does not list" >&2
            status=1
          fi
        done
      }
      check_wrapper ${nixglWrapper "nixGLIntel"} ${lib.escapeShellArgs nixglEnvironment.gl}
      check_wrapper ${nixglWrapper "nixVulkanIntel"} ${lib.escapeShellArgs nixglEnvironment.vulkan}
      [[ $status == 0 ]]
    '';
  };
  zed-host-environment = mkCheck {
    name = "zed-host-environment-test";
    tools = [pkgs.python3 pkgs.fish];
    script = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"
      python3 - ${settingsFile} ${nixglEnvironmentFile} <<'PYTHON'
      import json
      import os
      import subprocess
      import sys
      from pathlib import Path

      settings = json.loads(Path(sys.argv[1]).read_text())
      # Zed runs under both wrappers; nixgl-environment holds these lists to
      # what the wrappers export.
      environment = json.loads(Path(sys.argv[2]).read_text())
      graphics = sorted({*environment["gl"], *environment["vulkan"]})
      assert graphics, environment
      inherited = os.environ | dict.fromkeys(graphics, "/unusable-graphics-path")
      inherited["ZED_TEST_PRESERVED"] = "ordinary host environment"
      Path("probe.py").write_text(
          "import json, os, sys\n"
          "print(json.dumps({'env': dict(os.environ), 'args': sys.argv[1:]}))\n"
      )
      commands = {
          "terminal": [settings["terminal"]["shell"]["program"], "--no-config", "-c",
                       'exec python3 probe.py $argv', "sp ace", "second"],
          "ACP": [settings["agent_servers"]["OpenCode"]["command"],
                  sys.executable, "probe.py", "sp ace", "second"],
      }
      for name, command in commands.items():
          result = subprocess.run(command, env=inherited, check=True, text=True,
                                  capture_output=True)
          observed = json.loads(result.stdout)
          assert not set(graphics).intersection(observed["env"]), (name, observed)
          assert observed["env"]["ZED_TEST_PRESERVED"] == inherited["ZED_TEST_PRESERVED"]
          assert observed["args"] == ["sp ace", "second"], (name, observed)
      failing_commands = {
          "terminal": [settings["terminal"]["shell"]["program"], "--no-config",
                       "-c", "exit 23"],
          "ACP": [settings["agent_servers"]["OpenCode"]["command"], sys.executable,
                  "-c", "raise SystemExit(23)"],
      }
      for name, command in failing_commands.items():
          rejected = subprocess.run(command, env=inherited)
          assert rejected.returncode == 23, (name, rejected.returncode)
      PYTHON
    '';
  };
  zed-settings-materializer = mkCheck {
    name = "zed-settings-materializer-test";
    tools = [
      pkgs.python3
      zedSettingsMaterializer
    ];
    script = ''
      ${lib.optionalString (!settingsCheckPrecedesWrites) ''
        echo "checkZedSettings must run after checkLinkTargets and before writeBoundary" >&2
        exit 1
      ''}
      mkdir work
      printf '%s\n' \
        '{' \
        '  "theme": {"mode": "dark"},' \
        '  "vim_mode": true' \
        '}' \
        >work/static.json
      printf '%s\n' \
        '{' \
        '  // Zed accepts JSON5 comments and trailing commas.' \
        '  theme: {font_size: 14, mode: "light"},' \
        '  runtime_only: "preserved",' \
        '}' \
        >work/settings.json

      # The check mode, which activation runs before any link is switched,
      # accepts what a real run accepts and writes nothing: a file that needs
      # rewriting keeps its bytes, and a missing one is not created.
      cp work/settings.json work/settings.before
      identity=$(stat -c '%i %y' work/settings.json)
      materialize-zed-settings --check work/static.json work/settings.json
      cmp work/settings.before work/settings.json
      [[ $identity == "$(stat -c '%i %y' work/settings.json)" ]]
      materialize-zed-settings --check work/static.json work/absent/settings.json
      test ! -e work/absent

      # refuses DESCRIPTION TARGET: the check mode and a real run both
      # reject TARGET.
      refuses() {
        local mode
        for mode in --check ""; do
          if materialize-zed-settings $mode work/static.json "$2"; then
            echo "accepted $1''${mode:+ in check mode}" >&2
            exit 1
          fi
        done
      }

      materialize-zed-settings work/static.json work/settings.json
      python3 - work/settings.json <<'PYTHON'
      import json
      import stat
      import sys
      from pathlib import Path

      path = Path(sys.argv[1])
      settings = json.loads(path.read_text(encoding="utf-8"))
      assert settings["theme"] == {"font_size": 14, "mode": "dark"}
      assert settings["runtime_only"] == "preserved"
      assert settings["vim_mode"] is True
      assert stat.S_IMODE(path.stat().st_mode) == 0o600
      PYTHON

      # Current settings are left in place: same bytes, same inode.
      before=$(sha256sum work/settings.json)
      inode=$(stat -c %i work/settings.json)
      materialize-zed-settings work/static.json work/settings.json
      [[ $before == "$(sha256sum work/settings.json)" ]]
      [[ $inode == "$(stat -c %i work/settings.json)" ]]

      # Current content with the wrong mode is still rewritten at 0600.
      chmod 0644 work/settings.json
      materialize-zed-settings work/static.json work/settings.json
      [[ $(stat -c %a work/settings.json) == 600 ]]
      [[ $before == "$(sha256sum work/settings.json)" ]]

      # A runtime file that is not a JSON object must be refused rather
      # than silently replaced by the declared keys.
      printf '[]\n' >work/array.json
      refuses "a non-object runtime settings file" work/array.json
      test "$(cat work/array.json)" = '[]'

      # Failed parsing must not discard runtime-only settings.
      printf '{runtime_only: "unfinished"\n' >work/invalid.json
      cp work/invalid.json work/invalid.before
      refuses "malformed runtime settings" work/invalid.json
      cmp work/invalid.before work/invalid.json

      # Neither a valid nor a dangling link grants ownership of its target.
      cp work/settings.json work/external.json
      cp work/external.json work/external.before
      ln -s "$PWD/work/external.json" work/linked.json
      refuses "symlinked runtime settings" work/linked.json
      test -L work/linked.json
      cmp work/external.before work/external.json
      ln -s "$PWD/work/missing.json" work/dangling.json
      refuses "a dangling runtime settings link" work/dangling.json
      test -L work/dangling.json
      test ! -e work/missing.json
      ln -s "$PWD/work" work/linked-dir
      refuses "a symlinked Zed settings directory" work/linked-dir/settings.json
    '';
  };
}
