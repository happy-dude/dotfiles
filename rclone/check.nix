{pkgs}: {
  rclone-org-watcher =
    pkgs.runCommand "rclone-org-watcher-test"
    {nativeBuildInputs = [pkgs.python3];}
    ''
      test "$(python3 ${./watch_org.py} classify notes.org)" = sync
      test "$(python3 ${./watch_org.py} classify org-roam.db)" = ignore
      test "$(python3 ${./watch_org.py} classify org-roam.bak/note.org)" = ignore
      test "$(python3 ${./watch_org.py} classify .#note.org)" = ignore
      touch "$out"
    '';
}
