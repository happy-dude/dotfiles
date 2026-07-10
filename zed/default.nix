{
  config,
  pkgs,
  ...
}: {
  # Zed editor. Migrated from the former `zed/` GNU Stow package.
  #
  # SINGLE SOURCE OF TRUTH: zed/.config/zed/settings.json. Nix reads it via
  # builtins.fromJSON below, and `stow zed` links it directly on non-Nix hosts —
  # one file, consistent with the rest of the repo (stow file is the source,
  # Nix references it). Edit the JSON directly.
  #
  # mutableUserSettings = true (module default, explicit here) lets Zed keep
  # rewriting ~/.config/zed/settings.json at runtime; HM re-seeds it from the
  # JSON on each `home-manager switch`.
  programs.zed-editor = {
    enable = true;

    # Manage settings only, not the binary: this machine runs Zed Preview, but
    # nixpkgs only packages stable `zed-editor` (no Preview channel). Installing
    # it here would give the wrong build and collide with the Preview install.
    # Keep using the self-managed Zed Preview binary; Nix just owns the config.
    package = null;

    mutableUserSettings = true;

    userSettings = builtins.fromJSON (builtins.readFile ./.config/zed/settings.json);
  };
}
