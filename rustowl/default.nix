{
  inputs,
  lib,
  pkgs,
  ...
}: {
  home.packages = [(import ./package.nix {inherit inputs lib pkgs;}).server];
}
