{
  homes,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  rimeHostFiles = import ./host-files.nix {inherit pkgs;};
  rimeStateManager = import ./state-manager.nix {inherit pkgs;};
  catppuccinThemeDir = "${pkgs.catppuccin-fcitx5}/share/fcitx5/themes";
  catppuccinThemeNames = builtins.attrNames (
    pkgs.lib.filterAttrs (_: type: type == "directory") (builtins.readDir catppuccinThemeDir)
  );
  themeTargets = map (name: "fcitx5/themes/${name}") catppuccinThemeNames;
  ownsThemes = home:
    builtins.all (target: builtins.hasAttr target home.config.xdg.dataFile) themeTargets
    && builtins.all (
      name:
        toString home.config.xdg.dataFile."fcitx5/themes/${name}".source
        == "${catppuccinThemeDir}/${name}"
    )
    catppuccinThemeNames
    && builtins.hasAttr "fcitx5/themes/plasma" home.config.xdg.dataFile
    && toString home.config.xdg.dataFile."fcitx5/themes/plasma".source
    == toString ./.local/share/fcitx5/themes/plasma
    && !builtins.hasAttr "fcitx5/themes" home.config.xdg.dataFile;
in {
  rime-state-manager = assert ownsThemes homes.schan;
  assert ownsThemes homes.stachan;
    mkCheck {
      name = "rime-state-manager-test";
      tools = [rimeStateManager];
      script = ''
        mkdir -p home source/subdir state
        printf '%s\n' stamp-v1 >stamp
        printf '%s\n' schema-v1 >source/subdir/schema.yaml
        HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
          rime-state-manager deploy \
            "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
            subdir/schema.yaml
        test -L home/.local/share/fcitx5/rime/subdir/schema.yaml

        mkdir -p home/.local/share/fcitx5/rime/build
        printf '%s\n' generated >home/.local/share/fcitx5/rime/build/schema.bin
        printf '%s\n' learned >home/.local/share/fcitx5/rime/user.yaml
        printf '%s\n' stamp-v2 >stamp
        printf '%s\n' schema-v2 >source/subdir/schema.yaml
        HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
          rime-state-manager deploy \
            "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
            subdir/schema.yaml
        test ! -e home/.local/share/fcitx5/rime/build
        grep -qx learned home/.local/share/fcitx5/rime/user.yaml
        grep -qx schema-v2 home/.local/share/fcitx5/rime/subdir/schema.yaml

        rm home/.local/share/fcitx5/rime/subdir/schema.yaml
        printf '%s\n' unmanaged \
          >home/.local/share/fcitx5/rime/subdir/schema.yaml
        if HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
          rime-state-manager deploy \
            "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
            subdir/schema.yaml; then
          echo "accepted an unmanaged Rime schema target" >&2
          exit 1
        fi
      '';
    };
  rime-host-files = mkCheck {
    name = "rime-host-files-test";
    tools = [rimeHostFiles];
    script = ''
      source_root="$PWD/source"
      home="$PWD/home"
      state="$PWD/state"
      marker_source="$PWD/marker"
      mkdir -p "$source_root/.config/fcitx5/conf"
      printf '%s\n' profile-v1 >"$source_root/.config/fcitx5/profile"
      printf '%s\n' classic-v1 \
        >"$source_root/.config/fcitx5/conf/classicui.conf"
      printf '%s\n' rime-v1 >"$source_root/.config/fcitx5/conf/rime.conf"
      printf '%s\n' home-manager-rime-v1 >"$marker_source"

      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      test -f "$home/.config/fcitx5/profile"
      test ! -L "$home/.config/fcitx5/profile"
      test "$(stat -c %a "$home/.config/fcitx5/profile")" = 644
      test ! -e "$home/.local/share/fcitx5/themes"

      printf '%s\n' runtime-edit >"$home/.config/fcitx5/profile"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      grep -qx runtime-edit "$home/.config/fcitx5/profile"

      printf '%s\n' classic-v2 \
        >"$source_root/.config/fcitx5/conf/classicui.conf"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      grep -qx classic-v2 "$home/.config/fcitx5/conf/classicui.conf"

      printf '%s\n' profile-v2 >"$source_root/.config/fcitx5/profile"
      if HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"; then
        echo "accepted conflicting Rime host-file updates" >&2
        exit 1
      fi

      migration_home="$PWD/migration-home"
      migration_state="$PWD/migration-state"
      mkdir -p "$migration_home/.local/share/fcitx5" \
        "$migration_state/rime"
      cp "$marker_source" \
        "$migration_state/rime/home-manager-ownership-v1"
      ln -s /nix/store/legacy-fcitx5-themes \
        "$migration_home/.local/share/fcitx5/themes"
      HOME="$migration_home" XDG_STATE_HOME="$migration_state" \
        rime-host-files migrate-theme-root "$marker_source"
      test ! -e "$migration_home/.local/share/fcitx5/themes"
      test ! -e "$migration_state/rime/home-manager-ownership-v1"

      mkdir -p "$migration_home/.local/share/fcitx5/themes"
      cp "$marker_source" \
        "$migration_state/rime/home-manager-ownership-v1"
      HOME="$migration_home" XDG_STATE_HOME="$migration_state" \
        rime-host-files migrate-theme-root "$marker_source"
      test -d "$migration_home/.local/share/fcitx5/themes"
      test ! -e "$migration_state/rime/home-manager-ownership-v1"

      rmdir "$migration_home/.local/share/fcitx5/themes"
      ln -s /tmp/unmanaged-fcitx5-themes \
        "$migration_home/.local/share/fcitx5/themes"
      cp "$marker_source" \
        "$migration_state/rime/home-manager-ownership-v1"
      if HOME="$migration_home" XDG_STATE_HOME="$migration_state" \
        rime-host-files migrate-theme-root "$marker_source"; then
        echo "migrated an unmanaged Rime theme link" >&2
        exit 1
      fi
      test -L "$migration_home/.local/share/fcitx5/themes"
      test -f "$migration_state/rime/home-manager-ownership-v1"

      rm "$migration_home/.local/share/fcitx5/themes" \
        "$migration_state/rime/home-manager-ownership-v1"
      ln -s /nix/store/unrecorded-fcitx5-themes \
        "$migration_home/.local/share/fcitx5/themes"
      if HOME="$migration_home" XDG_STATE_HOME="$migration_state" \
        rime-host-files migrate-theme-root "$marker_source"; then
        echo "migrated an unrecorded Rime theme link" >&2
        exit 1
      fi
      test -L "$migration_home/.local/share/fcitx5/themes"

      rm "$migration_home/.local/share/fcitx5/themes"
      mkdir "$migration_state/rime/home-manager-ownership-v1"
      if HOME="$migration_home" XDG_STATE_HOME="$migration_state" \
        rime-host-files migrate-theme-root "$marker_source"; then
        echo "accepted a malformed Rime ownership marker" >&2
        exit 1
      fi
      test -d "$migration_state/rime/home-manager-ownership-v1"
    '';
  };
  rime-lua = mkCheck {
    name = "dotfiles-rime-lua-tests";
    tools = [
      pkgs.findutils
      pkgs.lua
    ];
    script = ''
      find ${./.} -type f -name '*.lua' -exec luac -p {} +

      cd ${../.}
      lua rime/tests/cangjie5_colemak_remap.lua
      lua rime/tests/romanization.lua
    '';
  };
}
