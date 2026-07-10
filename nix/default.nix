{ inputs, pkgs, ... }:

{
  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    package = pkgs.nixVersions.latest;
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
}
