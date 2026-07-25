{
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  workflow = mkCheck {
    name = "dotfiles-workflow-check";
    tools = [
      pkgs.actionlint
      pkgs.findutils
      pkgs.pinact
    ];
    script = ''
      find ${self}/.github/workflows -type f \
        \( -name '*.yml' -o -name '*.yaml' \) \
        -exec actionlint {} +

      cd ${self}
      pinact run --check
    '';
  };
}
