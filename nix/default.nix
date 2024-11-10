{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixVersions.latest;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
}
