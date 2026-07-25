{pkgs}: let
  dotfilesFiles = import ../lib/python {inherit pkgs;};
in
  pkgs.writers.writePython3Bin
  "rime-state-manager"
  {libraries = [dotfilesFiles];}
  (builtins.readFile ./state_manager.py)
