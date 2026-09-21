# Facts about the machine a profile targets.
#
# Modules ask what a machine provides rather than which user owns it.
# flake.nix declares each machine's capability record; this module exposes
# it through options so other modules do not read the raw record.
{
  lib,
  profile,
  ...
}: let
  inherit (lib) mkOption types;
in {
  options.dotfiles.profile = {
    username = mkOption {
      type = types.str;
      description = "Login name of the user this profile belongs to.";
    };

    desktop = mkOption {
      type = types.enum ["gnome" "plasma"];
      description = "Desktop session this profile integrates with.";
    };

    nixPackage = mkOption {
      type = types.nullOr types.package;
      description = ''
        The Nix client Home Manager configures and validates settings
        against, or null when the host installs and upgrades Nix itself.
      '';
    };

    hostProvidedNix = mkOption {
      type = types.bool;
      description = ''
        The host installs and upgrades Nix itself, so Home Manager must not
        install another Nix client. User-level Nix settings remain managed.
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

  config = {
    dotfiles.profile = {
      inherit (profile) username desktop nixPackage hasRustup hasFlatpak usesFlatpakZed hasSolaar;
      hostProvidedNix = profile.nixPackage == null;
    };

    home = {
      inherit (profile) username;
      homeDirectory = "/home/${profile.username}";
    };
  };
}
