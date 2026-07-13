{pkgs}:
pkgs.writers.writePython3Bin
"rime-state-manager"
{}
(builtins.readFile ./state_manager.py)
