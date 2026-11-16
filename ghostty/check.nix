{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  nixglEnvironmentFile = pkgs.writeText "nixgl-environment.json" (
    builtins.toJSON (import ../lib/nixgl-environment.nix)
  );
  resources = [
    "share/applications/com.mitchellh.ghostty.desktop"
    "share/dbus-1/services/com.mitchellh.ghostty.service"
    "share/systemd/user/app-com.mitchellh.ghostty.service"
  ];
  # Exercise the bare-command repair even when the lock has absolute templates.
  # Retain the upstream binary and metadata, with a non-launch token sentinel.
  bareGhostty = pkgs.symlinkJoin {
    name = "ghostty-bare-launch-fixture";
    paths = [pkgs.ghostty];
    postBuild =
      lib.concatMapStringsSep "\n" (resource: ''
        substitute "$out/${resource}" "$out/${resource}.tmp" \
          --replace-quiet '${pkgs.ghostty}/bin/ghostty' 'ghostty'
        printf '\n# Preserve Exec=ghostty outside launch fields.\n' >> "$out/${resource}.tmp"
        mv "$out/${resource}.tmp" "$out/${resource}"
      '')
      resources;
  };
in {
  ghostty-activation = mkCheck {
    name = "ghostty-activation";
    tools = [pkgs.python3 pkgs.desktop-file-utils];
    script = lib.concatMapStringsSep "\n" (name: let
      config = homes.${name}.config;
      package = config.programs.ghostty.package;
      service = config.xdg.configFile."systemd/user/app-com.mitchellh.ghostty.service".source;
      fixture =
        (import ./default.nix {
          inherit config lib;
          pkgs = pkgs // {ghostty = bareGhostty;};
        }).programs.ghostty.package;
    in ''
      echo 'Checking Ghostty activation for ${name}'
      desktop-file-validate ${package}/share/applications/com.mitchellh.ghostty.desktop
      python3 ${./check_activation.py} ${package} ${service}

      echo 'Checking bare launch templates for ${name}'
      python3 ${./check_activation.py} ${fixture} \
        ${fixture}/share/systemd/user/app-com.mitchellh.ghostty.service
      ${lib.concatMapStringsSep "\n" (resource: ''
          substitute ${bareGhostty}/${resource} expected \
            --replace-quiet $'\nExec=ghostty ' $'\nExec=${fixture}/bin/ghostty ' \
            --replace-quiet $'\nTryExec=ghostty\n' $'\nTryExec=${fixture}/bin/ghostty\n' \
            --replace-quiet $'\nExecStart=ghostty ' $'\nExecStart=${fixture}/bin/ghostty '
          cmp expected ${fixture}/${resource}
        '')
        resources}
    '') (lib.attrNames homes);
  };
  # Ghostty runs under nixGLIntel, and its shell must not inherit that
  # wrapper's graphics variables; the nixgl-environment check holds the list
  # to what the wrapper exports.
  ghostty-shell-environment = mkCheck {
    name = "ghostty-shell-environment-test";
    tools = [pkgs.python3];
    script = lib.concatMapStringsSep "\n" (name: let
      config = homes.${name}.config;
      launcher = "${config.programs.ghostty.package}/bin/ghostty";
      # The settings type holds every value as a list; this one is the path
      # of the shell wrapper alone.
      command = lib.toList config.programs.ghostty.settings.command;
      shell = assert lib.length command == 1; lib.head command;
      nixGLIntel =
        lib.getExe
        config.targets.genericLinux.nixGL.packages.${pkgs.stdenv.hostPlatform.system}.nixGLIntel;
    in ''
      echo 'Checking the Ghostty shell environment for ${name}'
      # The launcher reaches nixGLIntel itself or through one more wrapper.
      if ! grep -qF ${nixGLIntel} ${launcher}; then
        wrapper=$(grep -oE '/nix/store/[^" ]*nixGL[^" ]*' ${launcher} | head -n 1)
        grep -qF ${nixGLIntel} "$wrapper"
      fi

      export HOME="$TMPDIR/home-${name}"
      mkdir -p "$HOME"
      python3 - ${shell} ${nixglEnvironmentFile} <<'PYTHON'
      import json
      import os
      import subprocess
      import sys
      from pathlib import Path

      shell = sys.argv[1]
      graphics = json.loads(Path(sys.argv[2]).read_text())["gl"]
      assert graphics
      inherited = os.environ | dict.fromkeys(graphics, "/unusable-graphics-path")
      inherited["GHOSTTY_TEST_PRESERVED"] = "ordinary host environment"
      Path("probe.py").write_text(
          "import json, os, sys\n"
          "print(json.dumps({'env': dict(os.environ), 'args': sys.argv[1:]}))\n"
      )
      result = subprocess.run(
          [shell, "--no-config", "-c", "exec python3 probe.py $argv", "sp ace", "second"],
          env=inherited, check=True, text=True, capture_output=True,
      )
      observed = json.loads(result.stdout)
      assert not set(graphics).intersection(observed["env"]), observed
      assert observed["env"]["GHOSTTY_TEST_PRESERVED"] == inherited["GHOSTTY_TEST_PRESERVED"]
      assert observed["args"] == ["sp ace", "second"], observed
      # The terminal's shell is a login shell, and its status is the wrapper's.
      subprocess.run([shell, "--no-config", "-c", "status is-login"], env=inherited, check=True)
      rejected = subprocess.run([shell, "--no-config", "-c", "exit 23"], env=inherited)
      assert rejected.returncode == 23, rejected.returncode
      PYTHON
    '') (lib.attrNames homes);
  };
}
