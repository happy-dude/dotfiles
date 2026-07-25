{
  lib,
  pkgs,
  inputs,
  ...
}: let
  rimeHostFiles = import ./host-files.nix {inherit pkgs;};
  rimeStateManager = import ./state-manager.nix {inherit pkgs;};
  localRimeDataDir = ./.local/share/fcitx5/rime;
  localFcitxThemesDir = ./.local/share/fcitx5/themes;

  catppuccinThemeDir = "${pkgs.catppuccin-fcitx5}/share/fcitx5/themes";
  catppuccinThemeNames = builtins.attrNames (
    lib.filterAttrs (_: type: type == "directory") (builtins.readDir catppuccinThemeDir)
  );
  themeFiles = builtins.listToAttrs (
    map (name: {
      name = "fcitx5/themes/${name}";
      value.source = "${catppuccinThemeDir}/${name}";
    })
    catppuccinThemeNames
    ++ [
      {
        name = "fcitx5/themes/plasma";
        value.source = localFcitxThemesDir + "/plasma";
      }
    ]
  );

  # Locked schema revisions take precedence over the retained local snapshot.
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

  # The prior custom deployment wrote this marker. Keep its exact payload only
  # to authorize the one-way migration to native Home Manager theme ownership.
  legacyOwnershipMarker = pkgs.writeText "rime-home-manager-ownership-v1" ''
    home-manager-rime-v1
  '';
  rimeRelativeArguments = lib.concatMapStringsSep " " (entry:
    lib.escapeShellArg entry.relative)
  rimeDataEntries;
in
  assert duplicateRimeDataTargetNames == []; {
    xdg.dataFile = themeFiles;

    # The old activation created the theme root itself, outside Home Manager's
    # file manifest. Remove only that marked Nix-store link before collision
    # checks so Home Manager can own the individual immutable theme directories.
    home.activation.rimeMigrateThemeRoot = lib.hm.dag.entryBefore ["checkLinkTargets"] ''
      $DRY_RUN_CMD ${lib.getExe rimeHostFiles} migrate-theme-root \
        ${lib.escapeShellArg legacyOwnershipMarker}
    '';

    home.activation.rimeHostFiles = lib.hm.dag.entryAfter ["linkGeneration"] ''
      $DRY_RUN_CMD ${lib.getExe rimeHostFiles} deploy ${lib.escapeShellArg ./.}
    '';

    home.activation.rimeSchemaBuild = lib.hm.dag.entryAfter ["rimeHostFiles"] ''
      $DRY_RUN_CMD ${lib.getExe rimeStateManager} deploy \
        ${lib.escapeShellArg rimeStaticData} \
        ${lib.escapeShellArg rimeDataStamp} \
        ${lib.escapeShellArg "${pkgs.systemd}/bin/busctl"} \
        ${rimeRelativeArguments}
    '';
  }
