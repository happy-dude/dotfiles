{
  pkgs,
  self,
}: {
  python =
    pkgs.runCommand "dotfiles-python-checks"
    {
      nativeBuildInputs = [
        pkgs.prettier
        pkgs.python3
        pkgs.ruff
      ];
    }
    ''
      ruff format --check --no-cache ${self}
      ruff check --no-cache ${self}
      PYTHONPYCACHEPREFIX="$TMPDIR/pycache" \
        python3 -m compileall -q ${self}
      python3 ${self}/scripts/test_commit_message_lint.py
      touch "$out"
    '';
}
