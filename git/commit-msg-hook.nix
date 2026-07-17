{pkgs}: let
  commitMessageLinter = pkgs.writeShellApplication {
    name = "lint-agent-commit-message";
    runtimeInputs = [
      pkgs.prettier
      pkgs.python3
    ];
    text = ''
      exec python3 ${../scripts/lint_commit_message.py} "$@"
    '';
  };
in
  pkgs.writeShellApplication {
    name = "commit-msg";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.git
      pkgs.gnugrep
    ];
    text = ''
      message_path=$1
      common_dir=$(git rev-parse --path-format=absolute --git-common-dir)
      local_hook="$common_dir/hooks/commit-msg"

      if [[ -x $local_hook && \
        $(readlink -f "$local_hook") != $(readlink -f "$0") ]]; then
        "$local_hook" "$message_path"
      fi

      if grep -q '^Assisted-by:' "$message_path"; then
        exec ${pkgs.lib.getExe commitMessageLinter} "$message_path"
      fi
    '';
  }
