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
      import sys
      from pathlib import Path

      sys.path.insert(0, "${./.}")
      from watch_org import event_path

      org = Path("/home/user/org")
      assert event_path(b"/home/user/org/notes.org\n", org) == Path("notes.org")
      # A filename that is not valid UTF-8 must still yield a relative path.
      import os
      latin1 = event_path(b"/home/user/org/caf\xe9.org\n", org)
      assert latin1 == Path(os.fsdecode(b"caf\xe9.org"))
      # Events outside the watched tree carry nothing to relate.
      assert event_path(b"/home/user/elsewhere/x.org\n", org) is None
      PYTHON
    '';
  };
}
