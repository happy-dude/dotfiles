{
  config,
  lib,
  pkgs,
  username,
  ...
}: let
  managedSettings = builtins.fromJSON (builtins.readFile ./.config/zed/settings.json);
  jsonFormat = pkgs.formats.json {};
  staticSettings = jsonFormat.generate "zed-user-settings" managedSettings;
  json5 = pkgs.python3Packages.toPythonApplication pkgs.python3Packages.json5;
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
      settings_dir=${lib.escapeShellArg "${flatpakConfigHome}/zed"}
      settings_path="$settings_dir/settings.json"
      mkdir -p -- "$settings_dir"

      if [[ -L "$settings_path" ]]; then
        echo "Refusing symlinked Zed settings: $settings_path" >&2
        exit 1
      fi
      if [[ -e "$settings_path" && ! -f "$settings_path" ]]; then
        echo "Refusing non-regular Zed settings: $settings_path" >&2
        exit 1
      fi

      if [[ -e "$settings_path" ]]; then
        if ! dynamic="$(${lib.getExe json5} --as-json "$settings_path" 2>/dev/null)"; then
          echo "Refusing malformed Zed settings: $settings_path" >&2
          exit 1
        fi
      else
        dynamic='{}'
      fi

      static="$(cat ${lib.escapeShellArg staticSettings})"
      merged="$(
        ${lib.getExe pkgs.jq} -n '$dynamic * $static' \
          --argjson dynamic "$dynamic" \
          --argjson static "$static"
      )"

      (
        temporary="$(mktemp "$settings_dir/.settings.json.XXXXXX")"
        trap 'rm -f -- "$temporary"' EXIT
        printf '%s\n' "$merged" >"$temporary"
        mv -f -- "$temporary" "$settings_path"
      )
    '';
  };
}
