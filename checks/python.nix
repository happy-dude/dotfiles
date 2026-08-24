{
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  python = mkCheck {
    name = "dotfiles-python-checks";
    tools = [
      pkgs.findutils
      pkgs.prettier
      pkgs.python3
      pkgs.ruff
    ];
    script = ''
      ruff check --no-cache ${self}
      # compileall exits zero even when it compiles nothing.
      find ${self} -name '*.py' -print -quit | grep -q .
      PYTHONPYCACHEPREFIX="$TMPDIR/pycache" python3 -m compileall -q ${self}
      shopt -s nullglob
      test_files=(${self}/scripts/test_*.py)
      ((''${#test_files[@]} > 0))
      for test_file in "''${test_files[@]}"; do
        python3 "$test_file"
      done
    '';
  };
}
