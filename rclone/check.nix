{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (pkgs) lib;
  units =
    (import ./default.nix {
      inherit pkgs lib;
      config = {
        home.homeDirectory = "home";
        xdg = {
          configHome = "config";
          cacheHome = "cache";
          stateHome = "state";
        };
      };
    }).systemd.user;
in {
  rclone-bisync-state = mkCheck {
    name = "rclone-bisync-state-test";
    tools = [pkgs.rclone];
    script = let
      sync = units.services.rclone-box-org-bisync;
      needsOptIn = unit:
        lib.all (condition: lib.elem condition unit.Unit.ConditionPathExists) [
          "config/rclone/rclone.conf"
          "state/rclone/bisync/org-bisync-ready"
          "!state/rclone/org-bisync-ready"
        ];
    in ''
      ${lib.optionalString (!(lib.all needsOptIn [sync units.services.rclone-box-org-watch units.paths.rclone-box-org-watch])) ''
        echo 'The sync, watcher and path must require the state opt-in and reject the legacy opt-in.' >&2
        exit 1
      ''}
      # Both configs define box: as a local alias. Only the one checked by
      # the unit may receive changes, regardless of the manager's environment.
      export HOME="$PWD/home" RCLONE_CONFIG="$PWD/other.conf"
      mkdir -p home/org remote/org config/rclone
      config=config/rclone/rclone.conf
      printf '[box]\ntype = alias\nremote = %s\n' "$PWD/remote" >"$config"
      printf '[box]\ntype = alias\nremote = %s\n' "$PWD/other" >"$RCLONE_CONFIG"
      filter=config/rclone/org-bisync.filter
      install -m600 ${./org-bisync.filter} "$filter"
      touch home/org/{RCLONE_TEST,keep.org,removed.org} remote/org/RCLONE_TEST
      sync=( ${sync.Service.ExecStart} )
      rclone bisync home/org box:org --config "$config" --workdir cache/rclone/bisync \
        --filters-file "$filter" --check-access --compare size,modtime,checksum --resync
      cp -a remote other
      cp -a other saved-other
      cp "$filter.md5" saved.md5
      rm home/org/removed.org
      if "''${sync[@]}"; then
        echo 'A scheduled command accepted an uninitialized state workdir.' >&2
        exit 1
      fi
      test -f remote/org/removed.org
      rmdir state/rclone/bisync
      mv -T cache/rclone/bisync state/rclone/bisync
      printf '\n# changed\n' >>"$filter"
      if "''${sync[@]}"; then
        echo 'The filter changed without a reviewed resync.' >&2
        exit 1
      fi
      test -f remote/org/removed.org
      install -m600 ${./org-bisync.filter} "$filter"
      mv remote/org/RCLONE_TEST saved-sentinel
      if "''${sync[@]}"; then
        echo 'A missing access-check file did not stop bisync.' >&2
        exit 1
      fi
      test -f remote/org/removed.org
      mv saved-sentinel remote/org/RCLONE_TEST
      "''${sync[@]}"
      test ! -e remote/org/removed.org
      test -f remote/org/keep.org
      cmp saved.md5 "$filter.md5"
      diff -qr saved-other other
    '';
  };

  rclone-org-watcher = mkCheck {
    name = "rclone-org-watcher-test";
    tools = [
      pkgs.python3
      pkgs.rclone
    ];
    script = ''
      test "$(python3 ${./watch_org.py} classify notes.org)" = sync
      test "$(python3 ${./watch_org.py} classify org-roam.db)" = ignore
      test "$(python3 ${./watch_org.py} classify org-roam.bak/note.org)" = ignore
      test "$(python3 ${./watch_org.py} classify .#note.org)" = ignore
      test "$(python3 ${./watch_org.py} classify notes.org~)" = ignore
      test "$(python3 ${./watch_org.py} classify .dir-locals.el)" = ignore
      test "$(python3 ${./watch_org.py} classify sub/.dir-locals.el)" = ignore

      # The bisync filter and should_sync must keep the same files, and every
      # filter rule must drop a sample on its own, so a new rule needs one.
      samples=(
        notes.org sub/notes.org image.png org-roam.db org-roam.db-wal
        org-roam.bak/note.org sub/org-roam.bak/note.org .dir-locals.el
        sub/.dir-locals.el .#note.org sub/.#note.org notes.org~
      )
      mkdir tree
      for sample in "''${samples[@]}"; do
        mkdir -p "tree/$(dirname "$sample")"
        touch "tree/$sample"
      done
      export RCLONE_CONFIG=/dev/null
      kept() {
        rclone lsf --recursive --files-only --filter-from "$1" tree | sort
      }
      kept ${./org-bisync.filter} >rclone-kept
      for sample in "''${samples[@]}"; do
        if [[ $(python3 ${./watch_org.py} classify "$sample") == sync ]]; then
          printf '%s\n' "$sample"
        fi
      done | sort >watcher-kept
      diff rclone-kept watcher-kept
      while read -r rule; do
        # rclone trims each line, then skips it when empty or a # or ; comment.
        [[ -z $rule || $rule == [#\;]* ]] && continue
        printf '%s\n' "$rule" >rule.filter
        if [[ $(kept rule.filter | wc -l) -eq ''${#samples[@]} ]]; then
          echo "no sample exercises filter rule: $rule" >&2
          exit 1
        fi
      done <${./org-bisync.filter}

      python3 - <<'PYTHON'
      import os
      import sys
      from io import BufferedReader, BytesIO
      from pathlib import Path

      sys.path.insert(0, "${./.}")
      from watch_org import event_paths, schedule, should_sync

      class FragmentedStream(BytesIO):
          def readinto(self, buffer):
              return super().readinto(memoryview(buffer)[:3])

      org = Path("/home/user/org")
      names = [
          b"notes.org",
          b"caf\xe9.org",
          b"sub\nfolder/notes.org",
          b"notes.org~\n",
          b"notes.org~",
      ]
      events = b"\0".join(b"/home/user/org/" + name for name in names)
      events += b"\0/home/user/elsewhere/x.org\0"
      expected = [Path(os.fsdecode(name)) for name in names]
      # Pipes can split anywhere, including inside a filename; retain every
      # filename byte whether records arrive fragmented or together.
      for stream_type in (BytesIO, FragmentedStream):
          with BufferedReader(stream_type(events)) as stream:
              assert list(event_paths(stream, org)) == expected
      assert [path for path in expected if should_sync(path)] == expected[:-1]
      with BufferedReader(BytesIO(b"/home/user/org/unfinished")) as stream:
          try:
              list(event_paths(stream, org))
          except ValueError:
              pass
          else:
              raise AssertionError("an incomplete event was accepted")
      # Expected active-unit collisions stay quiet; unexpected systemd-run
      # failures are forwarded to stderr.
      collision = Path("collision-systemd-run")
      collision.write_text(
          "#!/bin/sh\n"
          "echo 'Failed to start transient timer unit: Unit "
          "rclone-box-org-bisync-change.timer already exists.' >&2\n"
          "exit 1\n",
          encoding="utf-8",
      )
      collision.chmod(0o755)
      broken = Path("broken-systemd-run")
      broken.write_text(
          "#!/bin/sh\n"
          "echo 'Failed to connect to user scope bus' >&2\n"
          "exit 1\n",
          encoding="utf-8",
      )
      broken.chmod(0o755)
      import io
      import contextlib
      captured = io.StringIO()
      with contextlib.redirect_stderr(captured):
          schedule(str(collision.resolve()), "/bin/true")
      assert captured.getvalue() == ""
      with contextlib.redirect_stderr(captured):
          schedule(str(broken.resolve()), "/bin/true")
      assert "Failed to connect to user scope bus" in captured.getvalue()
      PYTHON

    '';
  };
}
