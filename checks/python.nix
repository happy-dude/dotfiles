{
  pkgs,
  self,
}: {
  python =
    pkgs.runCommand "dotfiles-python-checks"
    {
      nativeBuildInputs = [
        pkgs.python3
        pkgs.ruff
      ];
    }
    ''
      ruff format --check --no-cache ${self}
      ruff check --no-cache ${self}
      PYTHONPYCACHEPREFIX="$TMPDIR/pycache" \
        python3 -m compileall -q ${self}
      touch "$out"
    '';
}
