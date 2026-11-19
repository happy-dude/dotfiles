{
  homes,
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};

  checkHome = profile: home: let
    cfg = home.config;
    aerc = cfg.programs.aerc.package;
    directory = "${cfg.xdg.configHome}/aerc";
    deployed = file: cfg.home.file."${directory}/${file}".source;
  in ''
    echo "Checking aerc for ${profile}"
    export HOME="$TMPDIR/${profile}"
    mkdir -p "$HOME"
    PATH="${aerc}/libexec/aerc/filters:$PATH" \
      python3 ${./check_config.py} ${self}/aerc/.config/aerc/aerc.conf \
        ${deployed "aerc.conf"} ${deployed "binds.conf"} \
        ${deployed "accounts.conf"} ${lib.escapeShellArg directory}
    timeout -k 2s 45s bash ${./check_delete.sh} ${lib.getExe aerc} ${deployed "binds.conf"}
    ${lib.concatMapStringsSep "\n" (name: ''
      cmp ${./.config/aerc/templates}/${name} ${deployed "templates/${name}"}
      test -s ${deployed "templates/${name}"}
      test -z "$(head -n 1 ${deployed "templates/${name}"})"
    '') ["thanks" "quoted_thanks"]}
  '';
in {
  # aerc.conf exists twice: as the attribute set Home Manager deploys, because
  # it must read general.unsafe-accounts-conf from there, and as the file in
  # aerc's own format that documents the same settings. Nothing derives one
  # from the other, so this check refuses to let them disagree.
  aerc-config-mirror = mkCheck {
    name = "aerc-config-mirror";
    tools = [pkgs.pandoc pkgs.python3 pkgs.tmux];
    script = assert homes != {}; lib.concatStringsSep "\n" (lib.mapAttrsToList checkHome homes);
  };
}
