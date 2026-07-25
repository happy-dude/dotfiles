{
  lib,
  flatpak,
}: let
  settings = builtins.fromJSON (builtins.readFile ./.config/zed/settings.json);
in
  if flatpak
  then
    lib.recursiveUpdate settings {
      agent_servers.OpenCode = {
        args = ["opencode" "acp"];
        command = "/app/bin/host-spawn";
      };
    }
  else settings
