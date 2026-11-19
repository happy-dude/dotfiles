{
  homes,
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  profiles = lib.filterAttrs (_: home: home.config.dotfiles.profile.desktop == "plasma") homes;
  flatpakPath = ".var/app/org.kde.okular/config/okularpartrc";
  check = name: home: let
    inherit (home) config;
    inherit (config.programs.plasma) configFile file overrideConfig;
    hasOkular = lib.any (app: (app.appId or null) == "org.kde.okular") (config.services.flatpak.packages or []);
    okularFiles = [".config/okularpartrc"] ++ lib.optional hasOkular flatpakPath;
    files =
      {
        ".config/kcminputrc" = configFile.kcminputrc;
        ".config/okularpartrc" = configFile.okularpartrc;
      }
      // lib.optionalAttrs hasOkular {${flatpakPath} = file.${flatpakPath};};
    inherit (import "${self.inputs.plasma-manager}/lib/writeconfig.nix" {inherit config pkgs;}) writeConfig;
  in
    assert !overrideConfig;
    assert builtins.hasAttr flatpakPath file == hasOkular; ''
      (
        mkdir -p ${lib.escapeShellArg name}/.config
        cd ${lib.escapeShellArg name}
        export HOME="$PWD"
        cat > .config/kcminputrc <<'EOF'
      [Libinput][1133][16507][Logitech MX Vertical]
      LeftHanded=true
      ScrollFactor=1

      [Libinput][1452][613][Apple Inc. Magic Trackpad]
      TapToClick=false
      EOF
        for file in ${lib.escapeShellArgs okularFiles}; do
          mkdir -p "$(dirname "$file")"
          cat > "$file" <<'EOF'
      [PageView]
      MouseMode=Browse
      ShowScrollBars=false

      [Custom]
      OtherPreference=keep
      EOF
        done

        ${writeConfig files overrideConfig []}
        input_key() {
          kreadconfig6 --file "$PWD/.config/kcminputrc" --group Libinput \
            --group "$1" --group "$2" --group "$3" --key "$4"
        }
        test "$(input_key 1133 16507 'Logitech MX Vertical' LeftHanded)" = true
        test "$(input_key 1133 16507 'Logitech MX Vertical' ScrollFactor)" = 2
        test "$(input_key 1452 613 'Apple Inc. Magic Trackpad' TapToClick)" = false
        for file in ${lib.escapeShellArgs okularFiles}; do
          okular_key() { kreadconfig6 --file "$PWD/$file" --group "$1" --key "$2"; }
          test "$(okular_key 'Core Performance' TextHinting)" = Enabled
          test "$(okular_key 'Main View' ShowLeftPanel)" = false
          test "$(okular_key PageView MouseMode)" = TextSelect
          test "$(okular_key PageView ShowScrollBars)" = false
          test "$(okular_key Custom OtherPreference)" = keep
        done
      )
    '';
in {
  plasma-writable-settings = mkCheck {
    name = "plasma-writable-settings";
    tools = [pkgs.kdePackages.kconfig];
    script = lib.concatStringsSep "\n" (lib.mapAttrsToList check profiles);
  };
}
