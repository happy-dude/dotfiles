{
  config,
  lib,
  pkgs,
  username,
  ...
}: let
  zedSettingsMaterializer = import ./materializer.nix {inherit pkgs;};
  managedSettings = builtins.fromJSON (builtins.readFile ./.config/zed/settings.json);
  jsonFormat = pkgs.formats.json {};
  staticSettings = jsonFormat.generate "zed-user-settings" managedSettings;
  flatpakConfigHome = "${config.home.homeDirectory}/.var/app/dev.zed.Zed-Preview/config";
in {
  # zed/.config/zed/settings.json is the sole declarative source for managed
  # keys. The schan Flatpak target remains mutable: runtime-only keys survive,
  # while declared keys are reasserted during activation.
  #
  # Keep the upstream module disabled on schan: extension or MCP-derived
  # settings would also reactivate its host-XDG target.
  programs.zed-editor = lib.mkIf (username != "schan") {
    enable = true;

    # The Zed binary is externally managed; Home Manager owns only its settings.
    package = null;

    mutableUserSettings = true;
    userSettings = managedSettings;
  };

  home.activation = lib.mkIf (username == "schan") {
    zedFlatpakSettingsActivation = lib.hm.dag.entryAfter ["linkGeneration"] ''
      settings_path=${lib.escapeShellArg "${flatpakConfigHome}/zed/settings.json"}
      ${zedSettingsMaterializer}/bin/materialize-zed-settings \
        ${lib.escapeShellArg staticSettings} \
        "$settings_path"
    '';
  };
}
