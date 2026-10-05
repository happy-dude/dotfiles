{
  config,
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
  catppuccinThemeNames = import ./themes.nix {inherit lib;};
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

  # OpenCC configs live under opencc/; other JSON (editor and language-server
  # settings) is not Rime data.
  isRimeDataFile = dir: name:
    (lib.hasSuffix ".yaml" name
      || lib.hasSuffix ".txt" name
      || lib.hasSuffix ".lua" name
      || (baseNameOf dir == "opencc" && lib.hasSuffix ".json" name))
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
        else lib.optional (isRimeDataFile dir name) path
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

  rimeRelativeArguments = lib.concatMapStringsSep " " (entry:
    lib.escapeShellArg entry.relative)
  rimeDataEntries;

  # Fcitx rewrites notifications.conf when a notification is hidden, so it is
  # materialized with the other host files. Fcitx stores the option as a list,
  # one numbered key per entry; a flat `HiddenNotifications=` key is ignored.
  # Copy only .config/fcitx5: `./.` would put all of rime/ in the store.
  fcitxConfig = pkgs.runCommand "fcitx5-config" {} ''
    cp -r ${./.config/fcitx5} "$out"
    chmod u+w "$out/conf"
    cat >"$out/conf/notifications.conf" <<'EOF'
    [HiddenNotifications]
    0=${
      if config.dotfiles.profile.desktop == "plasma"
      then "wayland-diagnose-kde"
      else "wayland-diagnose-gnome"
    }
    EOF
  '';
in
  assert duplicateRimeDataTargetNames == []; {
    xdg.dataFile = themeFiles;

    home.activation.rimeHostFiles = lib.hm.dag.entryAfter ["linkGeneration"] ''
      $DRY_RUN_CMD ${lib.getExe rimeHostFiles} deploy \
        ${lib.escapeShellArg fcitxConfig}
    '';

    home.activation.rimeSchemaBuild = lib.hm.dag.entryAfter ["rimeHostFiles"] ''
      $DRY_RUN_CMD ${lib.getExe rimeStateManager} deploy \
        ${lib.escapeShellArg rimeStaticData} \
        ${lib.escapeShellArg rimeDataStamp} \
        ${lib.escapeShellArg "${pkgs.systemd}/bin/busctl"} \
        ${rimeRelativeArguments}
    '';
  }
