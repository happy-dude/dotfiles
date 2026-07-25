# Package a repository Python helper as an executable.
#
# Every helper here is activation or formatting glue that reads and rewrites
# files, so each one gets the shared dotfiles_files module without asking.
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
