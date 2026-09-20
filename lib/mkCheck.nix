# Build a check derivation from a script that fails loudly or succeeds.
#
# Check scripts own their assertions. The helper supplies their tools and
# creates the output path only after the script succeeds.
{pkgs}: {
  name,
  tools ? [],
  script,
}:
pkgs.runCommand name {nativeBuildInputs = tools;} ''
  ${script}
  touch "$out"
''
