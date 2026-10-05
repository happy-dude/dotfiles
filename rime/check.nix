{
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  rimeHostFiles = import ./host-files.nix {inherit pkgs;};
  rimeStateManager = import ./state-manager.nix {inherit pkgs;};
  catppuccinThemeDir = "${pkgs.catppuccin-fcitx5}/share/fcitx5/themes";
  catppuccinThemeNames = import ./themes.nix {inherit lib;};
in {
  # themes.nix states the theme names so evaluation never reads the package;
  # hold the statement against what the package actually ships.
  rime-theme-names = mkCheck {
    name = "rime-theme-names";
    tools = [
      pkgs.coreutils
      pkgs.diffutils
      pkgs.findutils
    ];
    script = ''
      diff \
        <(find ${catppuccinThemeDir} -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort) \
        <(printf '%s\n' ${lib.concatMapStringsSep " " lib.escapeShellArg catppuccinThemeNames} | sort)
      echo 'themes.nix matches ${pkgs.catppuccin-fcitx5.name}'
    '';
  };

  rime-state-manager = mkCheck {
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

      # A run killed mid-swap renames the live static tree aside without
      # installing the replacement; the next deploy must restore it rather
      # than leave Rime without its managed data.
      staticdir="home/.local/share/fcitx5/rime/.home-manager-static"
      test -d "$staticdir"
      mv "$staticdir" "$staticdir.home-manager-old"
      printf '%s\n' stamp-v3 >stamp
      printf '%s\n' schema-v3 >source/subdir/schema.yaml
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml
      test -d "$staticdir"
      test ! -e "$staticdir.home-manager-old"
      grep -qx schema-v3 home/.local/share/fcitx5/rime/subdir/schema.yaml
      grep -qx learned home/.local/share/fcitx5/rime/user.yaml

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

      rm -r home/.local/share/fcitx5/rime/subdir
      printf '%s\n' unmanaged >home/.local/share/fcitx5/rime/subdir
      printf '%s\n' stamp-v4 >stamp
      if HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml 2>err; then
        echo "accepted an unmanaged Rime parent path" >&2
        exit 1
      fi
      grep -q 'Refusing unmanaged Rime path' err
      grep -qx stamp-v3 state/rime/home-manager-source-stamp
    '';
  };
  rime-host-files = mkCheck {
    name = "rime-host-files-test";
    tools = [rimeHostFiles];
    script = ''
      source_root="$PWD/source/fcitx5"
      home="$PWD/home"
      state="$PWD/state"
      mkdir -p "$source_root/conf"
      printf '%s\n' profile-v1 >"$source_root/profile"
      printf '%s\n' classic-v1 >"$source_root/conf/classicui.conf"
      printf '%s\n' rime-v1 >"$source_root/conf/rime.conf"
      printf '%s\n' notifications-v1 >"$source_root/conf/notifications.conf"

      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      test -f "$home/.config/fcitx5/profile"
      test ! -L "$home/.config/fcitx5/profile"
      test "$(stat -c %a "$home/.config/fcitx5/profile")" = 644
      test ! -e "$home/.local/share/fcitx5/themes"

      # Fcitx reads $XDG_CONFIG_HOME when it is set to an absolute path; the
      # base-directory specification says a relative value is ignored.
      relocated="$PWD/relocated-config"
      HOME="$home" XDG_CONFIG_HOME="$relocated" \
        XDG_STATE_HOME="$PWD/relocated-state" \
        rime-host-files deploy "$source_root"
      test -f "$relocated/fcitx5/profile"
      mkdir relative-home
      (
        cd relative-home
        HOME="$PWD" XDG_CONFIG_HOME=relative XDG_STATE_HOME=relative-state \
          rime-host-files deploy "$source_root"
      )
      test -f relative-home/.config/fcitx5/profile
      test -f relative-home/.local/state/rime/host-config/profile
      test ! -e relative-home/relative
      test ! -e relative-home/relative-state

      printf '%s\n' runtime-edit >"$home/.config/fcitx5/profile"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      grep -qx runtime-edit "$home/.config/fcitx5/profile"

      printf '%s\n' classic-v2 >"$source_root/conf/classicui.conf"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      grep -qx classic-v2 "$home/.config/fcitx5/conf/classicui.conf"

      printf '%s\n' profile-v2 >"$source_root/profile"
      if HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"; then
        echo "accepted conflicting Rime host-file updates" >&2
        exit 1
      fi

      linked_home="$PWD/linked-home"
      mkdir -p "$linked_home/.config"
      ln -s "$source_root" "$linked_home/.config/fcitx5"
      if HOME="$linked_home" XDG_STATE_HOME="$PWD/linked-state" \
        rime-host-files deploy "$source_root"; then
        echo "deployed through a linked Fcitx config directory" >&2
        exit 1
      fi
      test ! -e "$PWD/linked-state"

      # Fcitx rewrites notifications.conf when a notification is hidden. On
      # the first deploy that copy is kept; the other host files still refuse
      # an unmanaged regular file.
      fresh_home="$PWD/fresh-home"
      mkdir -p "$fresh_home/.config/fcitx5/conf"
      printf '%s\n' hidden-by-fcitx \
        >"$fresh_home/.config/fcitx5/conf/notifications.conf"
      HOME="$fresh_home" XDG_STATE_HOME="$PWD/fresh-state" \
        rime-host-files deploy "$source_root"
      grep -qx hidden-by-fcitx \
        "$fresh_home/.config/fcitx5/conf/notifications.conf"
      other_home="$PWD/other-home"
      mkdir -p "$other_home/.config/fcitx5"
      printf '%s\n' local-profile >"$other_home/.config/fcitx5/profile"
      if HOME="$other_home" XDG_STATE_HOME="$PWD/other-state" \
        rime-host-files deploy "$source_root"; then
        echo "adopted an unmanaged Fcitx profile" >&2
        exit 1
      fi
    '';
  };
  rime-failure-recovery = mkCheck {
    name = "rime-failure-recovery-test";
    tools = [pkgs.python3];
    script = let
      helpers = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.unions [./host_files.py ./state_manager.py];
      };
    in ''
      PYTHONPATH=${../lib/python}:${helpers} \
        python3 ${./tests/test_failure_recovery.py}
    '';
  };
  rime-lua = mkCheck {
    name = "dotfiles-rime-lua-tests";
    tools = [
      pkgs.findutils
      pkgs.lua
    ];
    # Only the Lua sources, rooted at the repository so the tests' rime/...
    # package.path still resolves; ./. would copy all of the Rime data too.
    script = let
      luaSources = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.fileFilter (file: file.hasExt "lua") ./.;
      };
    in ''
      find ${luaSources} -type f -name '*.lua' -exec luac -p {} +

      cd ${luaSources}
      lua rime/tests/cangjie5_colemak_remap.lua
      lua rime/tests/romanization.lua
    '';
  };
}
