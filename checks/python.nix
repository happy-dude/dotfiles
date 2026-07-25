{
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
in {
  python = mkCheck {
    name = "dotfiles-python-checks";
    tools = [
      pkgs.prettier
      pkgs.python3
      pkgs.ruff
    ];
    script = ''
      ruff check --no-cache ${self}
      PYTHONPYCACHEPREFIX="$TMPDIR/pycache" \
        python3 -m compileall -q \
          ${self}/agents ${self}/lib ${self}/rclone ${self}/rime \
          ${self}/scripts ${self}/zed
      python3 ${self}/scripts/test_commit_message_lint.py
    '';
  };
}
