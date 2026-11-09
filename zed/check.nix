{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  zedSettingsMaterializer = import ./materializer.nix {inherit pkgs;};
  hostSettings = import ./settings.nix {
    inherit pkgs;
    inherit (pkgs) lib;
  };
  settingsFile = (pkgs.formats.json {}).generate "zed-settings-check.json" hostSettings;
in {
  zed-host-environment = mkCheck {
    name = "zed-host-environment-test";
    tools = [pkgs.python3 pkgs.fish];
    script = ''
      export HOME="$TMPDIR/home"
      mkdir -p "$HOME"
      python3 - ${settingsFile} <<'PYTHON'
      import json
      import os
      import subprocess
      import sys
      from pathlib import Path

      settings = json.loads(Path(sys.argv[1]).read_text())
      graphics = [
          "GBM_BACKENDS_PATH", "LIBGL_ALWAYS_SOFTWARE", "LIBGL_DRIVERS_PATH",
          "LIBVA_DRIVERS_PATH", "__EGL_VENDOR_LIBRARY_FILENAMES", "LD_LIBRARY_PATH",
          "VK_ICD_FILENAMES", "VK_LAYER_PATH",
      ]
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
      if materialize-zed-settings work/static.json work/array.json; then
        echo "replaced a non-object runtime settings file" >&2
        exit 1
      fi
      test "$(cat work/array.json)" = '[]'

      # Failed parsing must not discard runtime-only settings.
      printf '{runtime_only: "unfinished"\n' >work/invalid.json
      cp work/invalid.json work/invalid.before
      if materialize-zed-settings work/static.json work/invalid.json; then
        echo "replaced malformed runtime settings" >&2
        exit 1
      fi
      cmp work/invalid.before work/invalid.json

      # Neither a valid nor a dangling link grants ownership of its target.
      cp work/settings.json work/external.json
      cp work/external.json work/external.before
      ln -s "$PWD/work/external.json" work/linked.json
      if materialize-zed-settings work/static.json work/linked.json; then
        echo "accepted symlinked runtime settings" >&2
        exit 1
      fi
      test -L work/linked.json
      cmp work/external.before work/external.json
      ln -s "$PWD/work/missing.json" work/dangling.json
      if materialize-zed-settings work/static.json work/dangling.json; then
        echo "accepted dangling runtime settings link" >&2
        exit 1
      fi
      test -L work/dangling.json
      test ! -e work/missing.json
      ln -s "$PWD/work" work/linked-dir
      if materialize-zed-settings work/static.json work/linked-dir/settings.json; then
        echo "accepted a symlinked Zed settings directory" >&2
        exit 1
      fi
    '';
  };
}
