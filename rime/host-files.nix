{pkgs}:
pkgs.writers.writePython3Bin
"rime-host-files"
{}
(builtins.readFile ./host_files.py)
