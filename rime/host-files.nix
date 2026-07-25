{pkgs}:
import ../lib/python/mkScript.nix {inherit pkgs;} {
  name = "rime-host-files";
  source = ./host_files.py;
}
