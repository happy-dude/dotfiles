# Build a check derivation from a script that fails loudly or succeeds.
#
# A flake check only has to produce its output path, so every check ended
# with the same line creating one. Stating the tools and the script is the
# whole of what a check actually says.
{pkgs}: {
  name,
  tools ? [],
  script,
}:
pkgs.runCommand name {nativeBuildInputs = tools;} ''
  ${script}
  touch "$out"
''
