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
    "${name}: usesFlatpakZed is set without hasFlatpak — Zed's Flatpak settings need a Flatpak installation"
    ++ lib.optional (p.managePlasmaPanels && p.desktop != "plasma")
    "${name}: managePlasmaPanels is set on a ${p.desktop} profile — only Plasma profiles import plasma-manager"
    ++ lib.optional (p.desktop == "plasma" && !(builtins.hasAttr p.username (import ../plasma/machines.nix)))
    "${name}: Plasma profile has no plasma/machines.nix entry — plasma/default.nix cannot evaluate without its touchpads, mice, and xwaylandScale";

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
