{
  inputs,
  pkgs,
  ...
}: let
  roswell = pkgs.roswell.overrideAttrs (_: {
    src = inputs.roswell_src;
  });
in {
  home.packages = [roswell];
}
