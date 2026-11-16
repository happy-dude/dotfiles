{pkgs}:
import ../lib/python/mkScript.nix {inherit pkgs;} {
  name = "rime-reading-tables";
  source = ./reading_tables.py;
}
