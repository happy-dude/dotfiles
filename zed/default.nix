{
  config,
  lib,
  pkgs,
  ...
}: let
  zedSettingsMaterializer = import ./materializer.nix {inherit pkgs;};
  managedSettings = import ./settings.nix {
    inherit lib;
    flatpak = config.dotfiles.profile.usesFlatpakZed;
  };
  jsonFormat = pkgs.formats.json {};
  staticSettings = jsonFormat.generate "zed-user-settings" managedSettings;
  settingsPath =
    if config.dotfiles.profile.usesFlatpakZed
    then "${config.home.homeDirectory}/.var/app/dev.zed.Zed-Preview/config/zed/settings.json"
    else "${config.xdg.configHome}/zed/settings.json";
in {
  # zed/.config/zed/settings.json is the declarative source for managed keys.
  # settings.nix adapts commands that must cross schan's Flatpak boundary.
  # Both targets remain mutable: runtime-only keys survive, while declared
  # keys are reasserted during activation. Zed binaries remain externally managed.
  home.activation.zedSettingsActivation = lib.hm.dag.entryAfter ["linkGeneration"] ''
    $DRY_RUN_CMD ${zedSettingsMaterializer}/bin/materialize-zed-settings \
      ${lib.escapeShellArg staticSettings} \
      ${lib.escapeShellArg settingsPath}
  '';
}
