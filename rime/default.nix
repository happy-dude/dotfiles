{
  lib,
  pkgs,
  inputs,
  rimeDeployment,
  ...
}: let
  localRimeDataDir = ./.local/share/fcitx5/rime;

  # Locked schema revisions take precedence over the retained Stow snapshot
  # when Home Manager deploys this module.
  schemaSources = [
    inputs.rime_bopomofo
    inputs.rime_cangjie
    inputs.rime_cantonese
    inputs.rime_essay
    inputs.rime_jyutping
    inputs.rime_luna_pinyin
    inputs.rime_prelude
    inputs.rime_stroke
    inputs.rime_terra_pinyin
    inputs.rime_loengfan
  ];

  isRimeDataFile = name:
    (lib.hasSuffix ".yaml" name || lib.hasSuffix ".txt" name || lib.hasSuffix ".lua" name)
    && !(builtins.elem name [
      "installation.yaml"
      "recipe.yaml"
    ]);

  filesRecursively = dir: let
    entries = builtins.readDir dir;
  in
    lib.concatMap (
      name: let
        path = dir + ("/" + name);
      in
        if entries.${name} == "directory"
        then filesRecursively path
        else lib.optional (isRimeDataFile name) path
    ) (builtins.attrNames entries);

  relativeTo = source: path:
    builtins.unsafeDiscardStringContext (lib.removePrefix ((toString source) + "/") (toString path));

  sourceEntries = source:
    map (path: {
      inherit path;
      relative = relativeTo source path;
    }) (filesRecursively source);

  externalRimeDataEntries = lib.concatMap sourceEntries schemaSources;
  externalRimeDataPaths = map (entry: entry.relative) externalRimeDataEntries;

  localRimeDataEntries =
    map
    (path: {
      inherit path;
      relative = relativeTo localRimeDataDir path;
    })
    (
      lib.filter (
        path: let
          relative = relativeTo localRimeDataDir path;
        in
          relative != "zhwiki.dict.yaml" && !(builtins.elem relative externalRimeDataPaths)
      ) (filesRecursively localRimeDataDir)
    );

  rimeDataEntries =
    localRimeDataEntries
    ++ externalRimeDataEntries
    ++ [
      {
        relative = "zhwiki.dict.yaml";
        path = (toString pkgs.rime-zhwiki) + "/share/rime-data/zhwiki.dict.yaml";
      }
    ];

  rimeDataTargetNames = map (entry: "fcitx5/rime/" + entry.relative) rimeDataEntries;
  duplicateRimeDataTargetNames = lib.filter (
    target: builtins.length (lib.filter (name: name == target) rimeDataTargetNames) > 1
  ) (lib.unique rimeDataTargetNames);

  # The link farm declares every discovered source file as a Nix input.
  # Activation materializes it in the writable Rime directory so managed static
  # inputs can coexist with generated schemas, learned databases, and sync state.
  rimeStaticData = pkgs.linkFarm "rime-static-data" (
    map (entry: {
      name = entry.relative;
      path = entry.path;
    })
    rimeDataEntries
  );

  localRimeDataStamp =
    map (entry: {
      inherit (entry) relative;
      hash = builtins.hashFile "sha256" entry.path;
    })
    localRimeDataEntries;

  # Rime tracks generated schemas by source paths and timestamps. Record the
  # Nix sources separately so a Home Manager update can invalidate only its
  # generated build cache when the static data changes.
  rimeDataStamp = pkgs.writeText "rime-data-stamp" (
    builtins.toJSON {
      deployment = "home-visible-static-v1";
      local = localRimeDataStamp;
      schemaSources = map toString schemaSources;
      staticData = toString rimeStaticData;
      zhwiki = toString pkgs.rime-zhwiki;
    }
  );

  rimeOwnershipMarker = pkgs.writeText "rime-home-manager-ownership-v1" ''
    home-manager-rime-v1
  '';
in
  assert duplicateRimeDataTargetNames == []; {
    config = lib.mkMerge [
      {
        assertions = [
          {
            assertion = builtins.elem rimeDeployment [
              "nix"
              "stow"
            ];
            message = "rimeDeployment must be either `nix` or `stow`";
          }
        ];
      }
      (lib.mkIf (rimeDeployment == "nix") {
        home.activation.rimeClaimOwnership = lib.hm.dag.entryAfter ["linkGeneration"] ''
          rime_state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/rime"
          rime_marker="$rime_state_dir/home-manager-ownership-v1"

          ${pkgs.coreutils}/bin/mkdir -p "$rime_state_dir"

          if [ -L "$rime_marker" ] || { [ -e "$rime_marker" ] && [ ! -f "$rime_marker" ]; }; then
            echo "Refusing malformed Rime ownership marker: $rime_marker" >&2
            exit 1
          fi

          if [ -e "$rime_marker" ]; then
            if ! ${pkgs.diffutils}/bin/cmp -s "${rimeOwnershipMarker}" "$rime_marker"; then
              echo "Refusing unrecognized Rime ownership marker: $rime_marker" >&2
              exit 1
            fi
          else
            marker_tmp="$(${pkgs.coreutils}/bin/mktemp "$rime_state_dir/.home-manager-ownership-v1.XXXXXX")"
            ${pkgs.coreutils}/bin/install -m 0644 "${rimeOwnershipMarker}" "$marker_tmp"
            ${pkgs.coreutils}/bin/mv -T -- "$marker_tmp" "$rime_marker"
          fi
        '';

        home.activation.rimeHostFiles = lib.hm.dag.entryAfter ["rimeClaimOwnership"] ''
          link_rime_path() {
            source="$1"
            target="$2"

            if [ ! -e "$source" ]; then
              echo "Rime source does not exist: $source" >&2
              exit 1
            fi

            ${pkgs.coreutils}/bin/mkdir -p "$( ${pkgs.coreutils}/bin/dirname "$target" )"

            if [ -L "$target" ]; then
              actual="$(${pkgs.coreutils}/bin/readlink -m -- "$target")"
              expected="$(${pkgs.coreutils}/bin/readlink -m -- "$source")"
              if [ "$actual" != "$expected" ]; then
                echo "Refusing to replace unmanaged Rime link: $target" >&2
                exit 1
              fi
              return
            elif [ -e "$target" ]; then
              echo "Refusing to replace unmanaged Rime path: $target" >&2
              exit 1
            fi

            ${pkgs.coreutils}/bin/ln -s "$source" "$target"
          }

          rime_dotfiles="$HOME/dotfiles/rime"
          link_rime_path "$rime_dotfiles/.config/fcitx5/profile" "$HOME/.config/fcitx5/profile"
          link_rime_path "$rime_dotfiles/.config/fcitx5/conf/classicui.conf" "$HOME/.config/fcitx5/conf/classicui.conf"
          link_rime_path "$rime_dotfiles/.config/fcitx5/conf/rime.conf" "$HOME/.config/fcitx5/conf/rime.conf"
          link_rime_path "$rime_dotfiles/.local/share/fcitx5/themes" "$HOME/.local/share/fcitx5/themes"
        '';
        home.activation.rimeSchemaBuild = lib.hm.dag.entryAfter ["rimeHostFiles"] ''
          ensure_static_link() {
            source="$1"
            target="$2"

            if [ ! -e "$source" ]; then
              echo "Rime static source does not exist: $source" >&2
              exit 1
            fi

            ${pkgs.coreutils}/bin/mkdir -p "$( ${pkgs.coreutils}/bin/dirname "$target" )"

            if [ -L "$target" ]; then
              actual="$(${pkgs.coreutils}/bin/readlink -m -- "$target")"
              expected="$(${pkgs.coreutils}/bin/readlink -m -- "$source")"
              if [ "$actual" != "$expected" ]; then
                echo "Refusing to replace unmanaged Rime link: $target" >&2
                exit 1
              fi
              return
            elif [ -e "$target" ]; then
              echo "Refusing to replace unmanaged Rime path: $target" >&2
              exit 1
            fi

            ${pkgs.coreutils}/bin/ln -s "$source" "$target"
            rime_changed=1
          }

          rime_data_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/fcitx5/rime"
          rime_static_dir="$rime_data_dir/.home-manager-static"
          rime_state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/rime"
          rime_stamp="$rime_state_dir/home-manager-source-stamp"
          rime_changed=0

          if [ -L "$rime_static_dir" ] || { [ -e "$rime_static_dir" ] && [ ! -d "$rime_static_dir" ]; }; then
            echo "Refusing malformed Rime static path: $rime_static_dir" >&2
            exit 1
          fi

          if [ -L "$rime_stamp" ] || { [ -e "$rime_stamp" ] && [ ! -f "$rime_stamp" ]; }; then
            echo "Refusing malformed Rime source stamp: $rime_stamp" >&2
            exit 1
          fi

          if [ ! -d "$rime_static_dir" ] || ! ${pkgs.diffutils}/bin/cmp -s "${rimeDataStamp}" "$rime_stamp"; then
            echo -e "\e[32mRefreshing generated Rime schemas...\e[0m"
            ${pkgs.coreutils}/bin/mkdir -p "$rime_data_dir"

            static_root="$(${pkgs.coreutils}/bin/readlink -m -- "$rime_static_dir")"
            while IFS= read -r -d "" link; do
              actual="$(${pkgs.coreutils}/bin/readlink -m -- "$link")"
              case "$actual" in
              "$static_root"/*)
                ${pkgs.coreutils}/bin/rm -f -- "$link"
                ;;
              esac
            done < <(
              ${pkgs.findutils}/bin/find "$rime_data_dir" \
                -path "$rime_static_dir" -prune -o -type l -print0
            )

            ${pkgs.coreutils}/bin/rm -rf "$rime_static_dir"
            ${pkgs.coreutils}/bin/mkdir -p "$rime_static_dir"
            ${pkgs.coreutils}/bin/cp -aL "${rimeStaticData}/." "$rime_static_dir/"
            ${pkgs.coreutils}/bin/chmod -R u+w "$rime_static_dir"

            ${pkgs.coreutils}/bin/rm -rf "$rime_data_dir/build"
            ${pkgs.coreutils}/bin/mkdir -p "$rime_state_dir"
            stamp_tmp="$(${pkgs.coreutils}/bin/mktemp "$rime_state_dir/.home-manager-source-stamp.XXXXXX")"
            ${pkgs.coreutils}/bin/install -m 0644 "${rimeDataStamp}" "$stamp_tmp"
            ${pkgs.coreutils}/bin/mv -T -- "$stamp_tmp" "$rime_stamp"
            rime_changed=1
          fi

          ${lib.concatMapStringsSep "\n" (entry: ''
              ensure_static_link \
                "$rime_static_dir/${entry.relative}" \
                "$rime_data_dir/${entry.relative}"
            '')
            rimeDataEntries}

          if [ "$rime_changed" -eq 1 ]; then
            if ! ${pkgs.systemd}/bin/busctl --user call org.fcitx.Fcitx5 /controller \
              org.fcitx.Fcitx.Controller1 ReloadAddonConfig s rime; then
              echo -e "\e[33mRime will rebuild generated schemas when Fcitx starts.\e[0m"
            fi
          fi
        '';
      })
      (lib.mkIf (rimeDeployment == "stow") {
        home.activation.rimeReleaseHomeManagerFiles = lib.hm.dag.entryAfter ["linkGeneration"] ''
          validate_owned_link() {
            target="$1"
            expected="$2"

            if [ -L "$target" ]; then
              actual="$(${pkgs.coreutils}/bin/readlink -m -- "$target")"
              wanted="$(${pkgs.coreutils}/bin/readlink -m -- "$expected")"
              if [ "$actual" != "$wanted" ]; then
                echo "Refusing to release unmanaged Rime link: $target" >&2
                exit 1
              fi
            elif [ -e "$target" ]; then
              echo "Refusing to release unmanaged Rime path: $target" >&2
              exit 1
            fi
          }

          release_link() {
            target="$1"
            if [ -L "$target" ]; then
              ${pkgs.coreutils}/bin/rm -f -- "$target"
            fi
          }

          rime_dotfiles="$HOME/dotfiles/rime"
          rime_data_dir="''${XDG_DATA_HOME:-$HOME/.local/share}/fcitx5/rime"
          rime_static_dir="$rime_data_dir/.home-manager-static"
          rime_state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/rime"
          rime_stamp="$rime_state_dir/home-manager-source-stamp"
          rime_marker="$rime_state_dir/home-manager-ownership-v1"
          rime_owned=0
          static_present=0
          stamp_present=0
          owned_schema_links=()

          if [ -L "$rime_static_dir" ] || { [ -e "$rime_static_dir" ] && [ ! -d "$rime_static_dir" ]; }; then
            echo "Refusing malformed Rime static path: $rime_static_dir" >&2
            exit 1
          elif [ -d "$rime_static_dir" ]; then
            static_present=1
          fi

          if [ -L "$rime_stamp" ] || { [ -e "$rime_stamp" ] && [ ! -f "$rime_stamp" ]; }; then
            echo "Refusing malformed Rime source stamp: $rime_stamp" >&2
            exit 1
          elif [ -f "$rime_stamp" ]; then
            stamp_present=1
          fi

          if [ -L "$rime_marker" ] || { [ -e "$rime_marker" ] && [ ! -f "$rime_marker" ]; }; then
            echo "Refusing malformed Rime ownership marker: $rime_marker" >&2
            exit 1
          elif [ -f "$rime_marker" ]; then
            if ! ${pkgs.diffutils}/bin/cmp -s "${rimeOwnershipMarker}" "$rime_marker"; then
              echo "Refusing unrecognized Rime ownership marker: $rime_marker" >&2
              exit 1
            fi
            rime_owned=1
          fi

          if [ -d "$rime_data_dir" ]; then
            static_root="$(${pkgs.coreutils}/bin/readlink -m -- "$rime_static_dir")"
            while IFS= read -r -d "" link; do
              actual="$(${pkgs.coreutils}/bin/readlink -m -- "$link")"
              case "$actual" in
              "$static_root"/*)
                owned_schema_links+=("$link")
                ;;
              esac
            done < <(
              ${pkgs.findutils}/bin/find "$rime_data_dir" \
                -path "$rime_static_dir" -prune -o -type l -print0
            )
          fi

          if [ "$rime_owned" -eq 0 ]; then
            if { [ "$static_present" -eq 1 ] && [ "$stamp_present" -eq 1 ]; } ||
              [ "''${#owned_schema_links[@]}" -gt 0 ]; then
              rime_owned=1
            elif [ "$static_present" -eq 1 ] || [ "$stamp_present" -eq 1 ]; then
              echo "Rime has incomplete Home Manager ownership state; refusing automatic cleanup" >&2
              exit 1
            fi
          fi

          if [ "$rime_owned" -eq 1 ]; then
            validate_owned_link \
              "$HOME/.config/fcitx5/profile" \
              "$rime_dotfiles/.config/fcitx5/profile"
            validate_owned_link \
              "$HOME/.config/fcitx5/conf/classicui.conf" \
              "$rime_dotfiles/.config/fcitx5/conf/classicui.conf"
            validate_owned_link \
              "$HOME/.config/fcitx5/conf/rime.conf" \
              "$rime_dotfiles/.config/fcitx5/conf/rime.conf"
            validate_owned_link \
              "$HOME/.local/share/fcitx5/themes" \
              "$rime_dotfiles/.local/share/fcitx5/themes"

            ${lib.concatMapStringsSep "\n" (entry: ''
              validate_owned_link \
                "$rime_data_dir/${entry.relative}" \
                "$rime_static_dir/${entry.relative}"
            '')
            rimeDataEntries}

            release_link "$HOME/.config/fcitx5/profile"
            release_link "$HOME/.config/fcitx5/conf/classicui.conf"
            release_link "$HOME/.config/fcitx5/conf/rime.conf"
            release_link "$HOME/.local/share/fcitx5/themes"

            for link in "''${owned_schema_links[@]}"; do
              ${pkgs.coreutils}/bin/rm -f -- "$link"
            done

            if [ "$static_present" -eq 1 ]; then
              ${pkgs.coreutils}/bin/rm -rf -- "$rime_static_dir"
            fi
            ${pkgs.coreutils}/bin/rm -f -- "$rime_stamp"
            ${pkgs.coreutils}/bin/rm -f -- "$rime_marker"
          fi
        '';
      })
    ];
  }
