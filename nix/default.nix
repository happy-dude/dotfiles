{
  config,
  inputs,
  lib,
  nixPackage,
  ...
}: let
  inherit (config.dotfiles.profile) hostProvidedNix;
  localConfig = "${config.xdg.configHome}/nix/local.conf";
in {
  nix = {
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    package = nixPackage;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = lib.optionalString (!hostProvidedNix) ''
      !include ${localConfig}
    '';
    settings = lib.optionalAttrs (!hostProvidedNix) {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
  };

  home.sessionVariablesExtra = lib.mkIf hostProvidedNix (lib.mkForce ''
    # Native Nix provides its own shell integration. Retain the generic-Linux
    # TERM refresh without sourcing a second, Home Manager-provided Nix hook.
    export TERM="$TERM"
  '');

  # Home Manager requires a package to validate nix.settings. A host-provided
  # Nix reads the equivalent user configuration without adding another client
  # to PATH.
  xdg.configFile."nix/nix.conf" = lib.mkIf hostProvidedNix {
    text = ''
      extra-experimental-features = nix-command flakes
      !include ${localConfig}
    '';
  };
}
