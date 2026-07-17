{
  pkgs,
  self,
}: {
  workflow =
    pkgs.runCommand "dotfiles-workflow-check"
    {
      nativeBuildInputs = [
        pkgs.actionlint
        pkgs.findutils
        pkgs.pinact
      ];
    }
    ''
      find ${self}/.github/workflows -type f \
        \( -name '*.yml' -o -name '*.yaml' \) \
        -exec actionlint {} +

      cd ${self}
      pinact run --check

      touch "$out"
    '';
}
