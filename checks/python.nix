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
      ruff check --no-cache ${self}
      PYTHONPYCACHEPREFIX="$TMPDIR/pycache" \
        python3 -m compileall -q \
          ${self}/agents ${self}/rclone ${self}/rime ${self}/scripts ${self}/zed
      python3 ${self}/scripts/test_commit_message_lint.py
      touch "$out"
    '';
}
