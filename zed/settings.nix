{
  lib,
  pkgs,
}: let
  settings = builtins.fromJSON (builtins.readFile ./.config/zed/settings.json);
  hostCommand = lib.getExe (import ./host-command.nix {inherit pkgs;});
  shell = settings.terminal.shell.program;
  # Zed identifies the shell dialect by the launcher's basename.
  hostShell = pkgs.writeShellScriptBin (builtins.baseNameOf shell) ''
    exec ${hostCommand} ${lib.escapeShellArg shell} "$@"
  '';
in
  lib.recursiveUpdate settings {
    terminal.shell.program = lib.getExe hostShell;
    # Canonical ACP entries are custom commands with explicit command/args;
    # recursiveUpdate preserves their remaining native settings.
    agent_servers =
      lib.mapAttrs (_: server: {
        command = hostCommand;
        args = [server.command] ++ server.args;
      })
      settings.agent_servers;
  }
