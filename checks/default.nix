{
  homes,
  lib,
  pkgs,
  self,
  treefmtChecks,
}: let
  codex = import ../agents/codex.nix {inherit pkgs;};

  # Checks are addressed by name on the command line, so two sets contributing
  # the same name would silently discard one of them.
  mergeDisjoint = lib.foldl' (
    merged: set: let
      clashing = lib.intersectLists (lib.attrNames merged) (lib.attrNames set);
    in
      if clashing == []
      then merged // set
      else throw "duplicate check names: ${lib.concatStringsSep ", " clashing}"
  ) {};
in
  mergeDisjoint [
    treefmtChecks
    {
      codex-profile-materializer = codex.checks.profileMaterializer;
      codex-agent-directory-migration = codex.checks.agentDirectoryMigration;
      dictionaries = import ../dictionaries/check.nix {inherit homes lib pkgs;};
      opencode = import ../opencode/check.nix {inherit homes lib pkgs self;};
    }
    (import ./profiles.nix {inherit homes lib pkgs;})
    (import ./python.nix {inherit pkgs self;})
    (import ./secrets.nix {inherit pkgs self;})
    (import ./shell.nix {inherit lib pkgs self;})
    (import ./workflow.nix {inherit pkgs self;})
    (import ../aerc/check.nix {inherit homes lib pkgs self;})
    (import ../agents/check.nix {inherit pkgs;})
    (import ../emacs/check.nix {inherit homes lib pkgs self;})
    (import ../git/check.nix {inherit pkgs;})
    (import ../rclone/check.nix {inherit pkgs;})
    (import ../rime/check.nix {inherit homes lib pkgs;})
    (import ../vim/check.nix {inherit homes pkgs self;})
    (import ../yt-dlp/check.nix {inherit homes lib pkgs;})
    (import ../zed/check.nix {inherit pkgs;})
  ]
