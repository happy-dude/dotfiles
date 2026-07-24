{
  homes,
  lib,
  pkgs,
  self,
}: let
  codex = import ../agents/codex.nix {inherit pkgs;};
in
  {
    codex-profile-materializer = codex.checks.profileMaterializer;
    codex-agent-directory-migration = codex.checks.agentDirectoryMigration;
    opencode = import ../opencode/check.nix {inherit homes lib pkgs self;};
  }
  // (import ./python.nix {inherit pkgs self;})
  // (import ./secrets.nix {inherit pkgs self;})
  // (import ./shell.nix {inherit pkgs self;})
  // (import ./workflow.nix {inherit pkgs self;})
  // (import ../agents/check.nix {inherit pkgs;})
  // (import ../emacs/check.nix {inherit homes pkgs self;})
  // (import ../git/check.nix {inherit pkgs;})
  // (import ../rclone/check.nix {inherit pkgs;})
  // (import ../rime/check.nix {inherit homes pkgs;})
  // (import ../vim/check.nix {inherit homes pkgs;})
  // (import ../yt-dlp/check.nix {inherit homes pkgs;})
  // (import ../zed/check.nix {inherit pkgs;})
