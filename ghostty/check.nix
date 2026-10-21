{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
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
          inherit config;
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
}
