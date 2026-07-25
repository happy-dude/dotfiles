{pkgs}:
import ../lib/python/mkScript.nix {inherit pkgs;} {
  name = "rime-state-manager";
  source = ./state_manager.py;
}
