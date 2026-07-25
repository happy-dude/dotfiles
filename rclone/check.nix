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
    '';
  };
}
