# Assert every profile's capability record is internally consistent.
#
# Mapping over `homes` rather than naming schan/stachan means a third profile
# is covered automatically instead of silently escaping the invariants.
{
  homes,
  lib,
  pkgs,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};

  # Each invariant returns a (possibly empty) list of human-readable problems
  # for one profile; a violation names the profile and the rule it broke.
  problemsFor = name: home: let
    p = home.config.dotfiles.profile;
  in
    lib.optional (p.usesFlatpakZed && !p.hasFlatpak)
    "${name}: usesFlatpakZed is set without hasFlatpak — Zed's Flatpak settings need a Flatpak installation";

  problems = lib.concatLists (lib.mapAttrsToList problemsFor homes);
in {
  profile-invariants = mkCheck {
    name = "profile-invariants";
    script =
      if problems == []
      then ''
        echo 'profile invariants hold for: ${lib.concatStringsSep ", " (lib.attrNames homes)}'
      ''
      else ''
        ${lib.concatMapStringsSep "\n" (p: "echo ${lib.escapeShellArg p} >&2") problems}
        exit 1
      '';
  };
}
