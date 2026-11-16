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
  # Check the directory names in themes.nix against the installed package.
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

      rm -rf home/.local/share/fcitx5/rime
      ln -s "$PWD/source" home/.local/share/fcitx5/rime
      if HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml 2>err; then
        echo "accepted a symlinked Rime data directory" >&2
        exit 1
      fi
      grep -q 'Refusing malformed Rime path' err
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
      printf '%s\n' '[HiddenNotifications]' 0=wayland-diagnose-gnome \
        >"$source_root/conf/notifications.conf"

      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      test -f "$home/.config/fcitx5/profile"
      test ! -L "$home/.config/fcitx5/profile"
      test "$(stat -c %a "$home/.config/fcitx5/profile")" = 644
      test ! -e "$home/.local/share/fcitx5/themes"
      # A missing notifications.conf is created from the declaration. It has
      # no snapshot: Fcitx owns the file once it exists.
      cmp "$source_root/conf/notifications.conf" \
        "$home/.config/fcitx5/conf/notifications.conf"
      test "$(stat -c %a "$home/.config/fcitx5/conf/notifications.conf")" = 644
      test -f "$state/rime/host-config/profile"
      test ! -e "$state/rime/host-config/notifications.conf"

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
        rime-host-files deploy "$source_root" 2>err; then
        echo "accepted conflicting Rime host-file updates" >&2
        exit 1
      fi
      grep -q 'changed both declaratively and at runtime' err
      grep -qx "The declaration deployed last is recorded in $state/rime/host-config/profile." err

      # The refusal prints one command for each way out. Recording the new
      # declaration keeps the runtime file.
      keep="$(grep -x '  cp .*' err)"
      test "$keep" = "  cp $source_root/profile $state/rime/host-config/profile"
      eval "$keep"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      grep -qx runtime-edit "$home/.config/fcitx5/profile"

      # Removing the runtime file takes the declaration.
      printf '%s\n' profile-v3 >"$source_root/profile"
      if HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root" 2>err; then
        echo "accepted conflicting Rime host-file updates" >&2
        exit 1
      fi
      take="$(grep -x '  rm .*' err)"
      test "$take" = "  rm $home/.config/fcitx5/profile"
      eval "$take"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy "$source_root"
      grep -qx profile-v3 "$home/.config/fcitx5/profile"
      grep -qx profile-v3 "$state/rime/host-config/profile"

      linked_home="$PWD/linked-home"
      mkdir -p "$linked_home/.config"
      ln -s "$source_root" "$linked_home/.config/fcitx5"
      if HOME="$linked_home" XDG_STATE_HOME="$PWD/linked-state" \
        rime-host-files deploy "$source_root"; then
        echo "deployed through a linked Fcitx config directory" >&2
        exit 1
      fi
      test ! -e "$PWD/linked-state"

      # Fcitx rewrites notifications.conf whenever a notification is hidden,
      # so activation merges the declared entries into the file Fcitx wrote
      # and keeps everything else in it. A snapshot of the file left in the
      # state directory is removed.
      fresh_home="$PWD/fresh-home"
      fresh_state="$PWD/fresh-state"
      notifications="$fresh_home/.config/fcitx5/conf/notifications.conf"
      mkdir -p "$fresh_home/.config/fcitx5/conf" \
        "$fresh_state/rime/host-config"
      printf '%s\n' Top=level '[HiddenNotifications]' 0=hidden-by-fcitx "" \
        '[Other]' Key=value >"$notifications"
      printf '%s\n' stale >"$fresh_state/rime/host-config/notifications.conf"
      HOME="$fresh_home" XDG_STATE_HOME="$fresh_state" \
        rime-host-files deploy "$source_root"
      printf '%s\n' Top=level '[HiddenNotifications]' 0=hidden-by-fcitx \
        1=wayland-diagnose-gnome "" '[Other]' Key=value >expected
      diff expected "$notifications"
      test ! -e "$fresh_state/rime/host-config/notifications.conf"

      # A file that already holds the declared entries is not rewritten.
      inode="$(stat -c %i "$notifications")"
      HOME="$fresh_home" XDG_STATE_HOME="$fresh_state" \
        rime-host-files deploy "$source_root"
      test "$(stat -c %i "$notifications")" = "$inode"
      diff expected "$notifications"

      # A changed declaration adds its ID instead of refusing the file. The
      # ID it replaced stays: it cannot be told apart from one hidden in
      # Fcitx.
      printf '%s\n' '[HiddenNotifications]' 0=wayland-diagnose-kde \
        >"$source_root/conf/notifications.conf"
      HOME="$fresh_home" XDG_STATE_HOME="$fresh_state" \
        rime-host-files deploy "$source_root"
      printf '%s\n' Top=level '[HiddenNotifications]' 0=hidden-by-fcitx \
        1=wayland-diagnose-gnome 2=wayland-diagnose-kde "" '[Other]' \
        Key=value >expected
      diff expected "$notifications"

      # An entry Fcitx adds later survives the next deploy untouched.
      printf '%s\n' Top=level '[HiddenNotifications]' 0=hidden-by-fcitx \
        1=wayland-diagnose-gnome 2=wayland-diagnose-kde 3=added-by-fcitx "" \
        '[Other]' Key=value >"$notifications"
      cp "$notifications" expected
      inode="$(stat -c %i "$notifications")"
      HOME="$fresh_home" XDG_STATE_HOME="$fresh_state" \
        rime-host-files deploy "$source_root"
      test "$(stat -c %i "$notifications")" = "$inode"
      diff expected "$notifications"
      test ! -e "$fresh_state/rime/host-config/notifications.conf"

      # A file without the section gains it after the existing content.
      printf '%s\n' '[Other]' Key=value >"$notifications"
      HOME="$fresh_home" XDG_STATE_HOME="$fresh_state" \
        rime-host-files deploy "$source_root"
      printf '%s\n' '[Other]' Key=value "" '[HiddenNotifications]' \
        0=wayland-diagnose-kde >expected
      diff expected "$notifications"

      # The other host files still refuse an unmanaged regular file.
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
