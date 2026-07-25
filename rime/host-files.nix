{pkgs}: let
  dotfilesFiles = import ../lib/python {inherit pkgs;};
in
  pkgs.writers.writePython3Bin
  "rime-host-files"
  {libraries = [dotfilesFiles];}
  (builtins.readFile ./host_files.py)
