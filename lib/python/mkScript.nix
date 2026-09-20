# Package a repository Python helper as an executable.
#
# File-replacement helpers share dotfiles_files; callers can add libraries
# without repeating that common dependency.
{pkgs}: let
  dotfilesFiles = import ./. {inherit pkgs;};
in
  {
    name,
    source,
    libraries ? [],
  }:
    pkgs.writers.writePython3Bin
    name
    {libraries = [dotfilesFiles] ++ libraries;}
    (builtins.readFile source)
