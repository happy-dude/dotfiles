{
  config,
  lib,
  pkgs,
  inputs,
  rimeDeployment,
  ...
}:

let
  localRimeDataDir = ./.local/share/fcitx5/rime;

  # These are the Rime schema repositories previously installed by Plum.
  # Their locked flake revisions take precedence over the retained Stow
  # snapshot when Home Manager deploys this module.
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
    && !(builtins.elem name [ "installation.yaml" "recipe.yaml" ]);

  filesRecursively = dir:
    let
      entries = builtins.readDir dir;
    in
    lib.concatMap (
      name:
      let
        path = dir + ("/" + name);
      in
      if entries.${name} == "directory" then
        filesRecursively path
      else
        lib.optional (isRimeDataFile name) path
    ) (builtins.attrNames entries);

  relativeTo = source: path:
    builtins.unsafeDiscardStringContext (lib.removePrefix ((toString source) + "/") (toString path));

  sourceEntries = source:
    map (
      path: {
        inherit path;
        relative = relativeTo source path;
      }
    ) (filesRecursively source);

  externalRimeDataEntries = lib.concatMap sourceEntries schemaSources;
  externalRimeDataPaths = map (entry: entry.relative) externalRimeDataEntries;

  localRimeDataEntries = map (
    path: {
      inherit path;
      relative = relativeTo localRimeDataDir path;
    }
  ) (
    lib.filter (
      path:
      let
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

  # The link farm declares every discovered source file as a Nix input. The
  # activation below dereferences it while copying to the shared home directory,
  # so host Fcitx never has to resolve a Toolbox-only /nix/store path.
  rimeStaticData = pkgs.linkFarm "rime-static-data" (
    map (entry: {
      name = entry.relative;
      path = entry.path;
    }) rimeDataEntries
  );

  localRimeDataStamp = map (
    entry: {
      inherit (entry) relative;
      hash = builtins.hashFile "sha256" entry.path;
    }
  ) localRimeDataEntries;

  # Rime tracks generated schemas by source paths and timestamps. Record the
  # Nix sources separately so a Home Manager update can invalidate only its
  # generated build cache when the static data changes.
  rimeDataStamp = pkgs.writeText "rime-data-stamp" (builtins.toJSON {
    deployment = "home-visible-static-v1";
    local = localRimeDataStamp;
    schemaSources = map toString schemaSources;
    staticData = toString rimeStaticData;
    zhwiki = toString pkgs.rime-zhwiki;
  });
in
assert duplicateRimeDataTargetNames == [ ];
{
  config = lib.mkMerge [
    {
      assertions = [
        {
          assertion = builtins.elem rimeDeployment [ "nix" "stow" ];
          message = "rimeDeployment must be either `nix` or `stow`";
        }
      ];
    }
    (lib.mkIf (rimeDeployment == "nix") {
      xdg = {
        configFile = {
          "fcitx5/profile".source =
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/rime/.config/fcitx5/profile";
          "fcitx5/conf/classicui.conf".source =
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/rime/.config/fcitx5/conf/classicui.conf";
          "fcitx5/conf/rime.conf".source =
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/rime/.config/fcitx5/conf/rime.conf";
        };

        dataFile = {
          "fcitx5/themes".source =
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/rime/.local/share/fcitx5/themes";
        };
      };

      home.activation.rimeSchemaBuild = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
        rime_data_dir="$HOME/.local/share/fcitx5/rime"
        rime_static_dir="$rime_data_dir/.home-manager-static"
        rime_state_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/rime"
        rime_stamp="$rime_state_dir/home-manager-source-stamp"

        if ! ${pkgs.diffutils}/bin/cmp -s "${rimeDataStamp}" "$rime_stamp"; then
          echo -e "\e[32mRefreshing generated Rime schemas...\e[0m"
          ${pkgs.coreutils}/bin/mkdir -p "$rime_data_dir"
          ${pkgs.findutils}/bin/find "$rime_data_dir" -type l -lname "$rime_static_dir/*" -delete
          ${pkgs.coreutils}/bin/rm -rf "$rime_static_dir"
          ${pkgs.coreutils}/bin/mkdir -p "$rime_static_dir"
          ${pkgs.coreutils}/bin/cp -aL "${rimeStaticData}/." "$rime_static_dir/"
          ${pkgs.coreutils}/bin/chmod -R u+w "$rime_static_dir"

          ${lib.concatMapStringsSep "\n" (
            entry: ''
              target="$rime_data_dir/${entry.relative}"
              ${pkgs.coreutils}/bin/mkdir -p "$( ${pkgs.coreutils}/bin/dirname "$target" )"
              ${pkgs.coreutils}/bin/rm -f "$target"
              ${pkgs.coreutils}/bin/ln -s "$rime_static_dir/${entry.relative}" "$target"
            ''
          ) rimeDataEntries}

          ${pkgs.coreutils}/bin/rm -rf "$rime_data_dir/build"
          ${pkgs.coreutils}/bin/mkdir -p "$rime_state_dir"
          ${pkgs.coreutils}/bin/rm -f "$rime_stamp"
          ${pkgs.coreutils}/bin/install -m 0644 "${rimeDataStamp}" "$rime_stamp"

          if ! ${pkgs.systemd}/bin/busctl --user call org.fcitx.Fcitx5 /controller \
            org.fcitx.Fcitx.Controller1 ReloadAddonConfig s rime; then
            echo -e "\e[33mRime will rebuild generated schemas when Fcitx starts.\e[0m"
          fi
        fi
      '';
    })
  ];
}
