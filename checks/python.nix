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
      python3 ${self}/scripts/test_commit_message_lint.py
    '';
  };
}
