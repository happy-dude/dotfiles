{pkgs}:
import ../lib/python/mkScript.nix {inherit pkgs;} {
  name = "adopt-opencode-cli";
  source = ./adopt_cli.py;
}
