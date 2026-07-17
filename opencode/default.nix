{
  lib,
  pkgs,
  ...
}: let
  prompts = import ../agents/prompts.nix {inherit lib;};
  json = pkgs.formats.json {};
  settings = {
    "$schema" = "https://opencode.ai/config.json";
    autoupdate = false;
    share = "disabled";
    permission = {
      bash = "ask";
      edit = "allow";
      external_directory = "ask";
    };
    agent =
      lib.mapAttrs (_: prompt: {
        inherit (prompt) description;
        mode = "all";
        prompt = prompt.body;
      })
      prompts;
  };
in {
  home.packages = [pkgs.opencode];
  xdg.configFile."opencode/opencode.json".source =
    json.generate "opencode.json" settings;
}
