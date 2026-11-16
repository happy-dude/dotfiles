{
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  rimeHostFiles = import ./host-files.nix {inherit pkgs;};
  rimeReadingTables = import ./reading-tables.nix {inherit pkgs;};
  rimeStateManager = import ./state-manager.nix {inherit pkgs;};
  catppuccinThemeDir = "${pkgs.catppuccin-fcitx5}/share/fcitx5/themes";
  catppuccinThemeNames = import ./themes.nix {inherit lib;};
  opencc = import ./opencc.nix {inherit lib;};

  # The Lua that librime-lua links, which is the one the filters and the
  # processor run under where Nix installs Fcitx. It is that package's own
  # input and an older release than pkgs.lua.
  rimeLua =
    lib.findFirst (package: lib.getName package == "lua")
    (throw "librime-lua no longer propagates a Lua interpreter")
    pkgs.librime-lua.propagatedBuildInputs;

  # Known answers for opencc.problems. The files are written at evaluation
  # time because the function reads them then.
  openccFixture = files:
    lib.mapAttrsToList (relative: text: {
      inherit relative;
      path = builtins.toFile (baseNameOf relative) text;
    })
    files;
  openccCases = [
    {
      name = "nested keys, patch paths, comments, and dictionaries";
      absent = ["absent.json"];
      entries =
        openccFixture {
          "a.schema.yaml" = ''
            simplifier:
              opencc_config: t2s.json
            variants:
              opencc_config: local.json  # provided below
            emoji:
              opencc_config: absent.json
            gone:
              opencc_config: gone.json
            # commented:
            #   opencc_config: commented.json
          '';
          "a.custom.yaml" = ''
            patch:
              "variants/opencc_config": 'quoted.json'
              gone/opencc_config: bare.json
          '';
          "opencc/local.json" = "{}";
        }
        ++ [
          # Never read: a dictionary names no configs.
          {
            relative = "big.dict.yaml";
            path = "/nonexistent/big.dict.yaml";
          }
        ];
      expected = [
        "a.custom.yaml names quoted.json, which no source provides"
        "a.custom.yaml names bare.json, which no source provides"
        "a.schema.yaml names gone.json, which no source provides"
      ];
    }
    {
      name = "an allowance that a source now provides";
      absent = ["absent.json"];
      entries = openccFixture {
        "a.schema.yaml" = "emoji:\n  opencc_config: absent.json\n";
        "opencc/absent.json" = "{}";
      };
      expected = ["absentOnPurpose lists absent.json, which a source now provides"];
    }
    {
      name = "an allowance that nothing names";
      absent = ["absent.json"];
      entries = openccFixture {
        "a.schema.yaml" = "simplifier:\n  opencc_config: t2s.json\n";
      };
      expected = ["absentOnPurpose lists absent.json, which no deployed file names any more"];
    }
  ];
  openccFailures =
    lib.concatMap (
      case: let
        actual = opencc.problems {inherit (case) absent entries;};
      in
        lib.optional (actual != case.expected)
        "${case.name}: expected ${builtins.toJSON case.expected}, got ${builtins.toJSON actual}"
    )
    openccCases;

  # Known answers for stockSchemas.problems.
  stockSchemas = import ./stock-schemas.nix {inherit lib;};
  stockSchemaEntries = [
    {
      relative = "sample.schema.yaml";
      path = builtins.toFile "sample.schema.yaml" ''
        # A comment, then the lists as the stock schemas write them.
        schema:
          schema_id: sample
          dependencies:
            - luna_pinyin  # 反查
            - stroke

        engine:
          processors:
            - ascii_composer
            # A comment between two items.
            - recognizer
          filters: [ simplifier, uniquifier ]
          translators:
            - punct_translator
              continued on a second line
            - script_translator

        filters:
          - not/engine
      '';
    }
  ];
  stockSchemaCases = [
    {
      name = "block lists, comments, and a blank line";
      expected."sample.schema.yaml" = {
        "schema/dependencies" = ["luna_pinyin" "stroke"];
        "engine/processors" = ["ascii_composer" "recognizer"];
      };
      problems = [];
    }
    {
      name = "a changed list, lists this reader cannot follow, and a file that is gone";
      expected = {
        "sample.schema.yaml" = {
          "engine/filters" = ["simplifier" "uniquifier"];
          "engine/processors" = ["recognizer" "ascii_composer"];
          "engine/segmentors" = ["abc_segmentor"];
          "engine/translators" = ["punct_translator" "script_translator"];
        };
        "gone.schema.yaml"."engine/filters" = [];
      };
      problems = [
        "gone.schema.yaml is not deployed"
        "sample.schema.yaml: engine/filters is missing or not a block list; expected [simplifier, uniquifier]"
        "sample.schema.yaml: engine/processors is [ascii_composer, recognizer]; expected [recognizer, ascii_composer]"
        "sample.schema.yaml: engine/segmentors is missing or not a block list; expected [abc_segmentor]"
        "sample.schema.yaml: engine/translators is [punct_translator]; expected [punct_translator, script_translator]"
      ];
    }
  ];
  stockSchemaFailures =
    lib.concatMap (
      case: let
        actual = stockSchemas.problems {
          inherit (case) expected;
          entries = stockSchemaEntries;
        };
      in
        lib.optional (actual != case.problems)
        "${case.name}: expected ${builtins.toJSON case.problems}, got ${builtins.toJSON actual}"
    )
    stockSchemaCases;

  # Shell for the validation-mode tests. `listing` prints one line per path
  # that changes when the path is created, removed, replaced, or rewritten.
  # `refused_alike` expects `validate` and `deploy`, which each check defines
  # for its helper, to refuse with the same message and to leave the paths
  # named in `watched` as they were.
  validationShell = ''
    listing() {
      { find "$@" -printf '%p %y %m %i %s %T@ %l\n' 2>/dev/null || true; } |
        sort
    }
    refused_alike() {
      listing "''${watched[@]}" >before
      if validate "$@" 2>validate-err; then
        echo "validation accepted what it must refuse: $*" >&2
        exit 1
      fi
      listing "''${watched[@]}" >after
      diff before after
      if deploy "$@" 2>err; then
        echo "deployment accepted what it must refuse: $*" >&2
        exit 1
      fi
      listing "''${watched[@]}" >after
      diff before after
      diff validate-err err
    }
  '';
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

  # Check the stock config names in opencc.nix against the installed
  # package, and opencc.problems against known answers. The deployed files
  # themselves are checked when rime/default.nix is evaluated.
  rime-opencc-configs = mkCheck {
    name = "rime-opencc-configs";
    tools = [
      pkgs.coreutils
      pkgs.diffutils
      pkgs.findutils
    ];
    script = ''
      diff \
        <(find ${pkgs.opencc}/share/opencc -maxdepth 1 -name '*.json' -printf '%f\n' | sort) \
        <(printf '%s\n' ${lib.concatMapStringsSep " " lib.escapeShellArg opencc.stockConfigs} | sort)
      echo 'opencc.nix matches ${pkgs.opencc.name}'
      ${
        if openccFailures == []
        then "echo 'opencc.problems gives the known answers'"
        else ''
          ${lib.concatMapStringsSep "\n" (failure: "echo ${lib.escapeShellArg failure} >&2") openccFailures}
          exit 1
        ''
      }
    '';
  };

  # stockSchemas.problems against known answers. The locked schemas
  # themselves are held to rime/stock-schemas.nix when rime/default.nix is
  # evaluated.
  rime-stock-schemas = mkCheck {
    name = "rime-stock-schemas";
    script =
      if stockSchemaFailures == []
      then "echo 'stockSchemas.problems gives the known answers'"
      else ''
        ${lib.concatMapStringsSep "\n" (failure: "echo ${lib.escapeShellArg failure} >&2") stockSchemaFailures}
        exit 1
      '';
  };

  rime-state-manager = mkCheck {
    name = "rime-state-manager-test";
    tools = [rimeStateManager];
    script = ''
      ${validationShell}
      # Activation validates before Home Manager switches links and deploys
      # after, so a refusal has to come from validation and write nothing.
      validate() {
        HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
          rime-state-manager validate "$PWD/source" "$PWD/stamp" "$@"
      }
      deploy() {
        HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
          rime-state-manager deploy \
            "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true "$@"
      }
      watched=(home state)

      mkdir -p home source/subdir state
      printf '%s\n' stamp-v1 >stamp
      printf '%s\n' schema-v1 >source/subdir/schema.yaml
      validate subdir/schema.yaml
      test -z "$(find home state -mindepth 1)"
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
      # Validation passes with a refresh pending and leaves it pending.
      listing home state >before
      validate subdir/schema.yaml
      listing home state >after
      diff before after
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml
      test ! -e home/.local/share/fcitx5/rime/build
      grep -qx learned home/.local/share/fcitx5/rime/user.yaml
      grep -qx schema-v2 home/.local/share/fcitx5/rime/subdir/schema.yaml

      # A run killed during the copy leaves its staging tree beside the live
      # one, with the read-only modes of the store path it was copying. The
      # next deploy removes it and replaces the live tree as usual.
      staticdir="home/.local/share/fcitx5/rime/.home-manager-static"
      test -d "$staticdir"
      mkdir -p "$staticdir.killed1/subdir"
      printf '%s\n' partial >"$staticdir.killed1/subdir/schema.yaml"
      chmod 555 "$staticdir.killed1/subdir" "$staticdir.killed1"
      printf '%s\n' stamp-v2b >stamp
      printf '%s\n' schema-v2b >source/subdir/schema.yaml
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml
      test ! -e "$staticdir.killed1"
      test ! -e "$staticdir.home-manager-old"
      grep -qx schema-v2b home/.local/share/fcitx5/rime/subdir/schema.yaml
      grep -qx learned home/.local/share/fcitx5/rime/user.yaml

      # A run killed mid-swap renames the live static tree aside without
      # installing the replacement; the next deploy must restore it rather
      # than leave Rime without its managed data. The staging tree that run
      # had finished copying is left behind as well.
      mv "$staticdir" "$staticdir.home-manager-old"
      mkdir -p "$staticdir.killed2/subdir"
      chmod 555 "$staticdir.killed2/subdir" "$staticdir.killed2"
      printf '%s\n' stamp-v3 >stamp
      printf '%s\n' schema-v3 >source/subdir/schema.yaml
      HOME="$PWD/home" XDG_STATE_HOME="$PWD/state" \
        rime-state-manager deploy \
          "$PWD/source" "$PWD/stamp" ${pkgs.coreutils}/bin/true \
          subdir/schema.yaml
      test -d "$staticdir"
      test ! -e "$staticdir.home-manager-old"
      test ! -e "$staticdir.killed2"
      grep -qx schema-v3 home/.local/share/fcitx5/rime/subdir/schema.yaml
      grep -qx learned home/.local/share/fcitx5/rime/user.yaml

      # A file the static source lacks is refused before the refresh, not
      # after it.
      printf '%s\n' stamp-v4 >stamp
      refused_alike subdir/schema.yaml subdir/missing.yaml
      grep -q 'Rime static source does not exist' err

      # An unmanaged regular file where a managed link belongs.
      rm home/.local/share/fcitx5/rime/subdir/schema.yaml
      printf '%s\n' unmanaged \
        >home/.local/share/fcitx5/rime/subdir/schema.yaml
      refused_alike subdir/schema.yaml
      grep -q 'Refusing to replace unmanaged Rime path' err

      # An unmanaged file where a managed directory belongs.
      rm -r home/.local/share/fcitx5/rime/subdir
      printf '%s\n' unmanaged >home/.local/share/fcitx5/rime/subdir
      refused_alike subdir/schema.yaml
      grep -q 'Refusing unmanaged Rime path' err
      grep -qx stamp-v3 state/rime/home-manager-source-stamp

      # A symlinked data directory.
      rm -rf home/.local/share/fcitx5/rime
      ln -s "$PWD/source" home/.local/share/fcitx5/rime
      refused_alike subdir/schema.yaml
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

      ${validationShell}
      # Activation validates before Home Manager switches links and deploys
      # after, so a refusal has to come from validation and write nothing.
      # Both take the home and state directories to act on.
      validate() {
        HOME="$1" XDG_STATE_HOME="$2" rime-host-files validate "$source_root"
      }
      deploy() {
        HOME="$1" XDG_STATE_HOME="$2" rime-host-files deploy "$source_root"
      }

      validate "$home" "$state"
      test ! -e "$home"
      test ! -e "$state"
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
      watched=("$home" "$state")
      refused_alike "$home" "$state"
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
      refused_alike "$home" "$state"
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
      watched=("$linked_home" "$PWD/linked-state" "$source_root")
      refused_alike "$linked_home" "$PWD/linked-state"
      grep -q 'Refusing to replace unmanaged Rime link' err
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
      # Validation passes with all of that pending and leaves it pending.
      listing "$fresh_home" "$fresh_state" >before
      validate "$fresh_home" "$fresh_state"
      listing "$fresh_home" "$fresh_state" >after
      diff before after
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
      watched=("$other_home" "$PWD/other-state")
      refused_alike "$other_home" "$PWD/other-state"
      grep -q 'Refusing unmanaged Rime host file' err

      # host_files.py names each file it materializes, while rime/default.nix
      # ships the whole tracked Fcitx tree. A file added to the tree and not
      # to the helper would be shipped and never written, so validation
      # refuses it, and the tracked tree has to pass. notifications.conf is
      # generated by rime/default.nix, not tracked.
      source_root="$PWD/tracked"
      cp -r ${./.config/fcitx5} "$source_root"
      chmod -R u+w "$source_root"
      printf '%s\n' '[HiddenNotifications]' 0=wayland-diagnose-gnome \
        >"$source_root/conf/notifications.conf"
      validate "$PWD/tracked-home" "$PWD/tracked-state"
      printf '%s\n' stray >"$source_root/conf/stray.conf"
      watched=("$PWD/tracked-home" "$PWD/tracked-state")
      refused_alike "$PWD/tracked-home" "$PWD/tracked-state"
      grep -q "Rime source file is not materialized: $source_root/conf/stray.conf" err
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
  # The generator's unit tests, then the packaged generator on the fixture
  # dictionaries. Its output has to be the tracked fixture module, which is
  # what the Lua tests load: the tables the filters are tested with are then
  # tables the generator really writes.
  rime-reading-tables = mkCheck {
    name = "rime-reading-tables-test";
    tools = [
      pkgs.diffutils
      pkgs.python3
      rimeReadingTables
    ];
    script = let
      sources = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.unions [
          ./reading_tables.py
          ./tests/test_reading_tables.py
          ./tests/fixtures
        ];
      };
      generate = command: output: ''
        ${command} \
          --pinyin rime/tests/fixtures/terra_pinyin.dict.yaml \
          --toneless-pinyin rime/tests/fixtures/luna_pinyin.dict.yaml \
          --jyutping rime/tests/fixtures/jyut6ping3.chars.dict.yaml \
          --jyutping rime/tests/fixtures/jyut6ping3.words.dict.yaml \
          --frequency rime/tests/fixtures/essay.txt \
          --min-frequency 500 \
          --output ${output}'';
    in ''
      PYTHONPATH=${../lib/python}:${sources} \
        python3 ${sources}/tests/test_reading_tables.py

      mkdir rime
      ln -s ${sources}/tests rime/tests
      ${generate "rime-reading-tables" "generated.lua"}
      if ! diff rime/tests/fixtures/reading_tables.generated generated.lua; then
        cat >&2 <<'EOF'
      rime/tests/fixtures/reading_tables.generated is not what the generator writes
      from the fixture dictionaries. Regenerate it from the repository root:
      ${generate "PYTHONPATH=lib/python python3 rime/reading_tables.py" "rime/tests/fixtures/reading_tables.generated"}
      EOF
        exit 1
      fi
    '';
  };
  rime-lua = mkCheck {
    name = "dotfiles-rime-lua-tests";
    tools = [
      pkgs.findutils
      pkgs.gnugrep
    ];
    # Only the Lua sources and the test fixtures, rooted at the repository so
    # the tests' rime/... package.path still resolves; ./. would copy all of
    # the Rime data too.
    script = let
      luaSources = lib.fileset.toSource {
        root = ../.;
        fileset = lib.fileset.union (lib.fileset.fileFilter (file: file.hasExt "lua") ./.) ./tests/fixtures;
      };
    in ''
      # rimeLua has to be the Lua the engine really loads.
      grep -qF ${rimeLua}/lib ${pkgs.librime}/lib/librime.so

      find ${luaSources} -type f -name '*.lua' \
        -exec ${rimeLua}/bin/luac -p {} +

      # Every test runs under the engine's Lua. A host that brings its own
      # Fcitx runs the same files under the Lua its librime-lua was built
      # with, which nothing here can see, so pkgs.lua, the newest release,
      # runs them too.
      cd ${luaSources}
      for lua in ${rimeLua}/bin/lua ${pkgs.lua}/bin/lua; do
        "$lua" -v
        for test in rime/tests/*.lua; do
          "$lua" "$test"
          echo "passed: $test"
        done
      done
    '';
  };
}
