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
      (pkgs.python3.withPackages (ps: [ps.pyyaml]))
    ];
    script = ''
      find ${self}/.github/workflows -type f \
        \( -name '*.yml' -o -name '*.yaml' \) \
        -exec actionlint {} +

      python3 - ${self}/.github/dependabot.yml <<'PYTHON'
      import sys
      from pathlib import Path

      import yaml

      config = yaml.safe_load(Path(sys.argv[1]).read_text(encoding="utf-8"))
      assert isinstance(config.get("updates"), list) and config["updates"]
      PYTHON

      cd ${self}
      pinact run --check
    '';
  };
}
