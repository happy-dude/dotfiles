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
          "fcitx5/profile".source = ./.config/fcitx5/profile;
          "fcitx5/conf/classicui.conf".source = ./.config/fcitx5/conf/classicui.conf;
          "fcitx5/conf/rime.conf".source = ./.config/fcitx5/conf/rime.conf;
        };

        # Link static files individually so Rime can create its writable build and
        # user-data directories beside them.
        dataFile = {
          "fcitx5/themes".source =
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/rime/.local/share/fcitx5/themes";
        }
        // lib.listToAttrs (
          map (
            entry: {
              name = "fcitx5/rime/" + entry.relative;
              value.source = entry.path;
            }
          ) rimeDataEntries
        );
      };
    })
  ];
}
