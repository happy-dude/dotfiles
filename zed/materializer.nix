{pkgs}:
pkgs.writers.writePython3Bin
"materialize-zed-settings"
{libraries = [pkgs.python3Packages.json5];}
(builtins.readFile ./materialize_settings.py)
