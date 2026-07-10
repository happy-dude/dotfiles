{
  inputs,
  lib,
  nixPackage,
  ...
}: {
  nix = {
    nixPath = ["nixpkgs=${inputs.nixpkgs}"];
    package = nixPackage;
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = lib.optionalAttrs (nixPackage != null) {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
    };
  };

  # Home Manager requires a package to validate nix.settings. A host-provided
  # Nix reads the equivalent user configuration without adding another client
  # to PATH.
  xdg.configFile."nix/nix.conf" = lib.mkIf (nixPackage == null) {
    text = ''
      extra-experimental-features = nix-command flakes
    '';
  };
}
