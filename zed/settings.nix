{
  lib,
  flatpak,
}: let
  settings = builtins.fromJSON (builtins.readFile ./.config/zed/settings.json);
  opencode = settings.agent_servers.OpenCode;
in
  # The Flatpak sandbox cannot run host executables directly, so its bundled
  # host-spawn runs the command settings.json declares, with the same args.
  if flatpak
  then
    lib.recursiveUpdate settings {
      agent_servers.OpenCode = {
        command = "/app/bin/host-spawn";
        args = [opencode.command] ++ opencode.args;
      };
    }
  else settings
