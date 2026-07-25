# Facts about the machine a profile targets.
#
# Modules ask what a machine provides rather than which user owns it, so
# adding a third profile means stating its facts here instead of finding
# every comparison against a username.
{
  lib,
  desktop,
  nixPackage,
  hasRustup,
  hasFlatpak,
  usesFlatpakZed,
  hasSolaar,
  ...
}: let
  inherit (lib) mkOption types;
in {
  options.dotfiles.profile = {
    desktop = mkOption {
      type = types.enum ["gnome" "plasma"];
      description = "Desktop session this profile integrates with.";
    };

    hostProvidedNix = mkOption {
      type = types.bool;
      description = ''
        The host installs and upgrades Nix itself, so Home Manager must
        neither install a Nix package nor own its configuration file.
      '';
    };

    hasRustup = mkOption {
      type = types.bool;
      description = ''
        A rustup toolchain is present outside Nix and wants shell
        completions.
      '';
    };

    hasFlatpak = mkOption {
      type = types.bool;
      description = ''
        A user Flatpak installation exists that Home Manager may manage.
      '';
    };

    usesFlatpakZed = mkOption {
      type = types.bool;
      description = ''
        Zed runs as a Flatpak, so its settings live under the sandbox
        configuration directory and host commands need host-spawn.
      '';
    };

    hasSolaar = mkOption {
      type = types.bool;
      description = "A Logitech receiver is attached and Solaar autostarts.";
    };
  };

  config.dotfiles.profile = {
    inherit desktop hasRustup hasFlatpak usesFlatpakZed hasSolaar;
    hostProvidedNix = nixPackage == null;
  };
}
