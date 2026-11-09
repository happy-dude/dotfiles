{
  config,
  lib,
  pkgs,
  ...
}: let
  zedSettingsMaterializer = import ./materializer.nix {inherit pkgs;};
  managedSettings = import ./settings.nix {
    inherit lib pkgs;
  };
  jsonFormat = pkgs.formats.json {};
  staticSettings = jsonFormat.generate "zed-user-settings" managedSettings;
  settingsPath = "${config.xdg.configHome}/zed/settings.json";
  # Vulkan stays local to Zed; enabling HM's global switch also changes the
  # graphics environment of every other nixGL-wrapped application.
  # Both profiles select Mesa; nixVulkanIntel supplies all Mesa ICDs.
  nixVulkan = config.targets.genericLinux.nixGL.packages.${pkgs.stdenv.hostPlatform.system}.nixVulkanIntel;
  zedPackage = (config.lib.nixGL.wrap pkgs.zed-editor).overrideAttrs (old: {
    # HM's generated launcher execs the original CLI by absolute path. Rename
    # only that script so the CLI keeps its executable-relative libexec lookup.
    buildCommand =
      old.buildCommand
      + ''
        mv "$out/bin/zeditor" "$out/bin/.zeditor-nixgl"
        makeWrapper ${lib.getExe nixVulkan} "$out/bin/zeditor" \
          --add-flags "$out/bin/.zeditor-nixgl"

        # Desktop launches must reach this wrapper without relying on PATH.
        for desktop in "$out/share/applications/"*.desktop; do
          sed -E "s@^(Exec|TryExec)=zeditor( |$)@\\1=$out/bin/zeditor\\2@" \
            "$desktop" > "$desktop.tmp"
          mv "$desktop.tmp" "$desktop"
          grep -q "^Exec=$out/bin/zeditor\( \|$\)" "$desktop"
          while IFS= read -r line; do
            case "$line" in
              Exec=*|TryExec=*)
                launch="''${line#*=}"
                case "$launch" in
                  "$out/bin/zeditor"|"$out/bin/zeditor "*) ;;
                  *) echo "Unwrapped Zed desktop launch: $desktop" >&2; exit 1 ;;
                esac
                ;;
            esac
          done < "$desktop"
        done
      '';
  });
in {
  home.packages = [zedPackage];

  # zed/.config/zed/settings.json is the declarative source for managed keys.
  # settings.nix keeps the graphics wrapper's environment out of the terminal
  # and declared ACP servers. Runtime-only settings survive activation; the
  # declared keys are reasserted in the native, writable settings file.
  home.activation.zedSettingsActivation = lib.hm.dag.entryAfter ["linkGeneration"] ''
    $DRY_RUN_CMD ${zedSettingsMaterializer}/bin/materialize-zed-settings \
      ${lib.escapeShellArg staticSettings} \
      ${lib.escapeShellArg settingsPath}
  '';
}
