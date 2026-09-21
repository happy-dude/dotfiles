{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  rclone-org-watcher = mkCheck {
    name = "rclone-org-watcher-test";
    tools = [pkgs.python3];
    script = ''
      test "$(python3 ${./watch_org.py} classify notes.org)" = sync
      test "$(python3 ${./watch_org.py} classify org-roam.db)" = ignore
      test "$(python3 ${./watch_org.py} classify org-roam.bak/note.org)" = ignore
      test "$(python3 ${./watch_org.py} classify .#note.org)" = ignore
      test "$(python3 ${./watch_org.py} classify notes.org~)" = ignore
      test "$(python3 ${./watch_org.py} classify .dir-locals.el)" = ignore
      test "$(python3 ${./watch_org.py} classify sub/.dir-locals.el)" = ignore

      python3 - <<'PYTHON'
      import os
      import sys
      from io import BufferedReader, BytesIO
      from pathlib import Path

      sys.path.insert(0, "${./.}")
      from watch_org import event_paths, should_sync

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
      PYTHON
    '';
  };
}
