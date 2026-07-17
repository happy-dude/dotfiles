{pkgs}: let
  rimeHostFiles = import ./host-files.nix {inherit pkgs;};
  rimeStateManager = import ./state-manager.nix {inherit pkgs;};
in {
  rime-state-manager =
    pkgs.runCommand "rime-state-manager-test"
    {nativeBuildInputs = [rimeStateManager];}
    ''
      mkdir -p home source/subdir state
      printf '%s\n' owned >marker
      printf '%s\n' stamp >stamp
      printf '%s\n' schema >source/subdir/schema.yaml
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager claim "$PWD/marker"
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml
      test -L home/.local/share/fcitx5/rime/subdir/schema.yaml
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager release \
          "$PWD/marker" ${pkgs.coreutils}/bin/true \
          "$PWD" "$PWD/source" subdir/schema.yaml
      test ! -e home/.local/share/fcitx5/rime/subdir/schema.yaml
      touch "$out"
    '';
  rime-host-files =
    pkgs.runCommand
    "rime-host-files-test"
    {nativeBuildInputs = [rimeHostFiles];}
    ''
      source_root="$PWD/source"
      home="$PWD/home"
      state="$PWD/state"
      mkdir -p "$source_root/.config/fcitx5/conf" \
        "$source_root/.local/share/fcitx5/themes"
      printf '%s\n' profile-v1 >"$source_root/.config/fcitx5/profile"
      printf '%s\n' classic-v1 \
        >"$source_root/.config/fcitx5/conf/classicui.conf"
      printf '%s\n' rime-v1 >"$source_root/.config/fcitx5/conf/rime.conf"

      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy \
          "$source_root" "$source_root/.local/share/fcitx5/themes"
      test -f "$home/.config/fcitx5/profile"
      test ! -L "$home/.config/fcitx5/profile"
      test "$(stat -c %a "$home/.config/fcitx5/profile")" = 644

      printf '%s\n' runtime-edit >"$home/.config/fcitx5/profile"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy \
          "$source_root" "$source_root/.local/share/fcitx5/themes"
      grep -qx runtime-edit "$home/.config/fcitx5/profile"

      printf '%s\n' classic-v2 \
        >"$source_root/.config/fcitx5/conf/classicui.conf"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy \
          "$source_root" "$source_root/.local/share/fcitx5/themes"
      grep -qx classic-v2 "$home/.config/fcitx5/conf/classicui.conf"

      printf '%s\n' profile-v2 >"$source_root/.config/fcitx5/profile"
      if HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files deploy \
          "$source_root" "$source_root/.local/share/fcitx5/themes"; then
        echo "accepted conflicting Rime host-file updates" >&2
        exit 1
      fi

      printf '%s\n' profile-v1 >"$source_root/.config/fcitx5/profile"
      if HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files release \
          "$source_root" "$source_root/.local/share/fcitx5/themes"; then
        echo "discarded a runtime-modified Rime host file" >&2
        exit 1
      fi
      printf '%s\n' profile-v1 >"$home/.config/fcitx5/profile"
      HOME="$home" XDG_STATE_HOME="$state" \
        rime-host-files release \
          "$source_root" "$source_root/.local/share/fcitx5/themes"
      test ! -e "$home/.config/fcitx5/profile"
      test ! -e "$home/.local/share/fcitx5/themes"
      touch "$out"
    '';
  rime-lua =
    pkgs.runCommand "dotfiles-rime-lua-tests"
    {
      nativeBuildInputs = [
        pkgs.findutils
        pkgs.lua
      ];
    }
    ''
      find ${./.} -type f -name '*.lua' -exec luac -p {} +

      cd ${../.}
      lua rime/tests/cangjie5_colemak_remap.lua
      lua rime/tests/romanization.lua

      touch "$out"
    '';
}
