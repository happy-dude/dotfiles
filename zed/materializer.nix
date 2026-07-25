{pkgs}: let
  dotfilesFiles = import ../lib/python {inherit pkgs;};
in
  pkgs.writers.writePython3Bin
  "materialize-zed-settings"
  {libraries = [dotfilesFiles pkgs.python3Packages.json5];}
  (builtins.readFile ./materialize_settings.py)
