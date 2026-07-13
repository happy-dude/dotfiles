{pkgs}:
pkgs.writeShellApplication {
  name = "rime-host-files";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.diffutils
  ];
  text = ''
    usage() {
      echo "usage: rime-host-files deploy|release SOURCE_ROOT THEME_SOURCE" >&2
      return 2
    }

    install_atomic() {
      local source=$1 target=$2 mode=$3 target_dir temporary
      target_dir=$(dirname -- "$target")
      mkdir -p -- "$target_dir"
      temporary=$(mktemp "$target_dir/.rime-host-file.XXXXXX")
      trap 'rm -f -- "$temporary"' RETURN
      install -m "$mode" -- "$source" "$temporary"
      mv -T -- "$temporary" "$target"
      trap - RETURN
    }

    validate_materialize_file() {
      local source=$1 target=$2 snapshot=$3 actual expected
      [[ -f $source && ! -L $source ]] || {
        echo "Rime source is not a regular file: $source" >&2
        return 1
      }
      if [[ -L $snapshot || (-e $snapshot && ! -f $snapshot) ]]; then
        echo "Refusing malformed Rime host snapshot: $snapshot" >&2
        return 1
      fi
      if [[ -L $target ]]; then
        actual=$(readlink -m -- "$target")
        expected=$(readlink -m -- "$source")
        if [[ $actual != "$expected" ]]; then
          echo "Refusing to replace unmanaged Rime link: $target" >&2
          return 1
        fi
      elif [[ -e $target && ! -f $target ]]; then
        echo "Refusing to replace unmanaged Rime path: $target" >&2
        return 1
      elif [[ -f $target && ! -e $snapshot ]] && ! cmp -s -- "$source" "$target"; then
        echo "Refusing unmanaged Rime host file: $target" >&2
        return 1
      elif [[ -f $target && -f $snapshot ]] &&
        ! cmp -s -- "$source" "$snapshot" &&
        ! cmp -s -- "$target" "$snapshot" &&
        ! cmp -s -- "$target" "$source"; then
        echo "Rime host file changed both declaratively and at runtime: $target" >&2
        return 1
      fi
    }

    materialize_file() {
      local source=$1 target=$2 snapshot=$3
      mkdir -p -- "$(dirname -- "$target")" "$(dirname -- "$snapshot")"
      if [[ -L $target ]]; then
        rm -f -- "$target"
      fi

      if [[ ! -e $target ]]; then
        install_atomic "$source" "$target" 0644
        install_atomic "$source" "$snapshot" 0600
      elif [[ ! -e $snapshot ]]; then
        install_atomic "$source" "$snapshot" 0600
        chmod 0644 -- "$target"
      elif cmp -s -- "$source" "$snapshot"; then
        chmod 0644 -- "$target"
      elif cmp -s -- "$target" "$snapshot"; then
        install_atomic "$source" "$target" 0644
        install_atomic "$source" "$snapshot" 0600
      elif cmp -s -- "$target" "$source"; then
        install_atomic "$source" "$snapshot" 0600
        chmod 0644 -- "$target"
      fi
    }

    validate_release_file() {
      local source=$1 target=$2 snapshot=$3 actual expected
      if [[ -L $target ]]; then
        actual=$(readlink -m -- "$target")
        expected=$(readlink -m -- "$source")
        if [[ $actual != "$expected" ]]; then
          echo "Refusing to release unmanaged Rime link: $target" >&2
          return 1
        fi
      elif [[ -e $target ]]; then
        if [[ ! -f $target || ! -f $snapshot || -L $snapshot ]]; then
          echo "Refusing to release unmanaged Rime path: $target" >&2
          return 1
        fi
        if ! cmp -s -- "$target" "$snapshot" && ! cmp -s -- "$target" "$source"; then
          echo "Refusing to discard runtime-modified Rime host file: $target" >&2
          return 1
        fi
      fi
    }

    release_file() {
      local target=$1 snapshot=$2
      rm -f -- "$target"
      rm -f -- "$snapshot"
    }

    validate_link() {
      local source=$1 target=$2 legacy_source=''${3:-} actual expected
      [[ -e $source ]] || {
        echo "Rime source does not exist: $source" >&2
        return 1
      }
      if [[ -L $target ]]; then
        actual=$(readlink -m -- "$target")
        expected=$(readlink -m -- "$source")
        if [[ $actual == "$expected" ]]; then
          return 0
        fi
        if [[ -n $legacy_source && $actual == "$(readlink -m -- "$legacy_source")" ]]; then
          return 0
        else
          echo "Refusing to replace unmanaged Rime link: $target" >&2
          return 1
        fi
      elif [[ -e $target ]]; then
        echo "Refusing to replace unmanaged Rime path: $target" >&2
        return 1
      fi
    }

    ensure_link() {
      local source=$1 target=$2 actual expected
      mkdir -p -- "$(dirname -- "$target")"
      if [[ -L $target ]]; then
        actual=$(readlink -m -- "$target")
        expected=$(readlink -m -- "$source")
        if [[ $actual == "$expected" ]]; then
          return 0
        fi
        rm -f -- "$target"
      fi
      ln -s -- "$source" "$target"
    }

    validate_release_link() {
      local source=$1 target=$2 actual expected
      if [[ -L $target ]]; then
        actual=$(readlink -m -- "$target")
        expected=$(readlink -m -- "$source")
        if [[ $actual != "$expected" ]]; then
          echo "Refusing to release unmanaged Rime link: $target" >&2
          return 1
        fi
      elif [[ -e $target ]]; then
        echo "Refusing to release unmanaged Rime path: $target" >&2
        return 1
      fi
    }

    [[ $# == 3 ]] || usage
    operation=$1
    source_root=$2
    theme_source=$3
    state_root="''${XDG_STATE_HOME:-$HOME/.local/state}/rime/host-config"

    case $operation in
    deploy)
      validate_materialize_file "$source_root/.config/fcitx5/profile" \
        "$HOME/.config/fcitx5/profile" "$state_root/profile"
      validate_materialize_file "$source_root/.config/fcitx5/conf/classicui.conf" \
        "$HOME/.config/fcitx5/conf/classicui.conf" "$state_root/classicui.conf"
      validate_materialize_file "$source_root/.config/fcitx5/conf/rime.conf" \
        "$HOME/.config/fcitx5/conf/rime.conf" "$state_root/rime.conf"
      validate_link "$theme_source" "$HOME/.local/share/fcitx5/themes" \
        "$source_root/.local/share/fcitx5/themes"
      materialize_file "$source_root/.config/fcitx5/profile" \
        "$HOME/.config/fcitx5/profile" "$state_root/profile"
      materialize_file "$source_root/.config/fcitx5/conf/classicui.conf" \
        "$HOME/.config/fcitx5/conf/classicui.conf" "$state_root/classicui.conf"
      materialize_file "$source_root/.config/fcitx5/conf/rime.conf" \
        "$HOME/.config/fcitx5/conf/rime.conf" "$state_root/rime.conf"
      ensure_link "$theme_source" "$HOME/.local/share/fcitx5/themes" \
        "$source_root/.local/share/fcitx5/themes"
      ;;
    release)
      validate_release_file "$source_root/.config/fcitx5/profile" \
        "$HOME/.config/fcitx5/profile" "$state_root/profile"
      validate_release_file "$source_root/.config/fcitx5/conf/classicui.conf" \
        "$HOME/.config/fcitx5/conf/classicui.conf" "$state_root/classicui.conf"
      validate_release_file "$source_root/.config/fcitx5/conf/rime.conf" \
        "$HOME/.config/fcitx5/conf/rime.conf" "$state_root/rime.conf"
      validate_release_link "$theme_source" "$HOME/.local/share/fcitx5/themes"
      release_file "$HOME/.config/fcitx5/profile" "$state_root/profile"
      release_file "$HOME/.config/fcitx5/conf/classicui.conf" "$state_root/classicui.conf"
      release_file "$HOME/.config/fcitx5/conf/rime.conf" "$state_root/rime.conf"
      rm -f -- "$HOME/.local/share/fcitx5/themes"
      ;;
    *) usage ;;
    esac
  '';
}
