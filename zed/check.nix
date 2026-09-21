{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
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
    mkCheck {
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
      '';
    };
}
