# Wire the offline StarDict dictionaries into sdcv.
#
# sdcv itself is installed by home.nix (Language agent tooling); this module
# only points it at the reproducible dictionary store path.  sdcv reads
# $STARDICT_DATA_DIR/dic, which is exactly the built tree.
{pkgs, ...}: let
  dictionaries = import ./package.nix {inherit pkgs;};
in {
  home.sessionVariables.STARDICT_DATA_DIR = "${dictionaries}";
}
