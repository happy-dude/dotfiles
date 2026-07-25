{pkgs}: let
  zedSettingsMaterializer = import ./materializer.nix {inherit pkgs;};
  flatpakSettings = import ./settings.nix {
    inherit (pkgs) lib;
    flatpak = true;
  };
  hostSettings = import ./settings.nix {
    inherit (pkgs) lib;
    flatpak = false;
  };
in {
  zed-settings-materializer = assert hostSettings.agent_servers.OpenCode
  == {
    args = ["acp"];
    command = "opencode";
    env = {};
    type = "custom";
  };
  assert flatpakSettings.agent_servers.OpenCode
  == {
    args = ["opencode" "acp"];
    command = "/app/bin/host-spawn";
    env = {};
    type = "custom";
  };
    pkgs.runCommand "zed-settings-materializer-test"
    {
      nativeBuildInputs = [
        pkgs.python3
        zedSettingsMaterializer
      ];
    }
    ''
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
      touch "$out"
    '';
}
