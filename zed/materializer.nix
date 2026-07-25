{pkgs}:
import ../lib/python/mkScript.nix {inherit pkgs;} {
  name = "materialize-zed-settings";
  source = ./materialize_settings.py;
  libraries = [pkgs.python3Packages.json5];
}
