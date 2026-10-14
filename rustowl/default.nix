{
  inputs,
  pkgs,
  ...
}: {
  home.packages = [(import ./package.nix {inherit inputs pkgs;}).server];
}
