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

  # Every deployed Rime data file has exactly one source: a locked schema
  # input, pkgs.rime-zhwiki, or the tracked tree under localRimeDataDir. The
  # tracked tree holds only what no input provides (the Lua, the custom
  # patches, and the Colemak schema); it is not a fallback copy of the inputs,
  # and a path with two sources fails evaluation below instead of one source
  # silently winning.
  schemaSourceNames = [
    "rime_bopomofo"
    "rime_cangjie"
    "rime_cantonese"
    "rime_essay"
    "rime_jyutping"
    "rime_luna_pinyin"
    "rime_prelude"
    "rime_stroke"
    "rime_terra_pinyin"
    "rime_loengfan"
  ];
  schemaSources = map (name: inputs.${name}) schemaSourceNames;

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

  # Hidden entries are repository metadata and build inputs (.github, .ci,
  # .build), never Rime data, so the walk skips them at every depth.
  filesRecursively = dir: let
    entries = builtins.readDir dir;
  in
    lib.concatMap (
      name: let
        path = dir + ("/" + name);
      in
        if lib.hasPrefix "." name
        then []
        else if entries.${name} == "directory"
        then filesRecursively path
        else lib.optional (isRimeDataFile dir name) path
    ) (builtins.attrNames entries);

  relativeTo = source: path:
    builtins.unsafeDiscardStringContext (lib.removePrefix ((toString source) + "/") (toString path));

  # `origin` names the source in the failure messages below.
  sourceEntries = origin: source:
    map (path: {
      inherit origin path;
      relative = relativeTo source path;
    }) (filesRecursively source);

  localRimeDataOrigin = "rime/.local/share/fcitx5/rime";
  localRimeDataEntries = sourceEntries localRimeDataOrigin localRimeDataDir;

  providedRimeDataEntries =
    lib.concatMap (name: sourceEntries name inputs.${name}) schemaSourceNames
    ++ [
      # No schema imports this dictionary; it stays installed on purpose.
      {
        origin = "pkgs.rime-zhwiki";
        relative = "zhwiki.dict.yaml";
        path = (toString pkgs.rime-zhwiki) + "/share/rime-data/zhwiki.dict.yaml";
      }
    ];

  rimeDataEntries = localRimeDataEntries ++ providedRimeDataEntries;

  # One line per deployed path that has more than one source, naming each.
  multiplySourced = entries:
    lib.mapAttrsToList (
      relative: sources: "  ${relative}: ${lib.concatMapStringsSep ", " (entry: entry.origin) sources}"
    ) (
      lib.filterAttrs (_: sources: builtins.length sources > 1) (
        lib.groupBy (entry: entry.relative) entries
      )
    );

  localRimeDataPaths = map (entry: entry.relative) localRimeDataEntries;
  shadowedLocalRimeData = multiplySourced (
    localRimeDataEntries
    ++ lib.filter (entry: builtins.elem entry.relative localRimeDataPaths) providedRimeDataEntries
  );
  duplicateProvidedRimeData = multiplySourced providedRimeDataEntries;

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

  # Fcitx rewrites notifications.conf when a notification is hidden, so
  # activation merges this declaration into the file instead of replacing it:
  # the declared IDs are added to whatever Fcitx recorded. Fcitx stores the
  # option as a list, one numbered key per entry; a flat
  # `HiddenNotifications=` key is ignored.
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
  assert lib.assertMsg (shadowedLocalRimeData == []) ''
    Tracked Rime data files that a locked input or package also provides:
    ${lib.concatStringsSep "\n" shadowedLocalRimeData}
    Only one source can be deployed. Remove the tracked copy with `git rm`; a
    local change to a file an input provides belongs in a `.custom.yaml` patch
    or in a file of another name.'';
  assert lib.assertMsg (duplicateProvidedRimeData == []) ''
    Rime data files that more than one locked input or package provides:
    ${lib.concatStringsSep "\n" duplicateProvidedRimeData}
    Only one source can be deployed; drop one of them from schemaSourceNames
    in rime/default.nix.''; {
    xdg.dataFile = themeFiles;

    # Both helpers refuse unmanaged or conflicting files, and their deploy
    # entries run after linkGeneration, where a refusal leaves the new
    # generation's links in place and the remaining entries unrun. Raise the
    # same refusals here, before anything is written, so a refusal leaves the
    # previous generation intact. Validation only reads, so it also runs in a
    # dry run.
    home.activation.rimeValidate = lib.hm.dag.entryBetween ["writeBoundary"] ["checkLinkTargets"] ''
      ${lib.getExe rimeHostFiles} validate \
        ${lib.escapeShellArg fcitxConfig}
      ${lib.getExe rimeStateManager} validate \
        ${lib.escapeShellArg rimeStaticData} \
        ${lib.escapeShellArg rimeDataStamp} \
        ${rimeRelativeArguments}
    '';

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
