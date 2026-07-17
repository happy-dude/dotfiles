{pkgs}: let
  commitMessageLinter = pkgs.writeShellApplication {
    name = "lint-commit-message";
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
      agent_assisted=false
      if grep -q '^Assisted-by:' "$message_path"; then
        agent_assisted=true
      fi

      lint_message() {
        if [[ $agent_assisted == true ]] &&
          ! grep -q '^Assisted-by:' "$message_path"; then
          printf '%s\n' \
            "agent-assisted messages must retain an Assisted-by trailer" >&2
          return 1
        fi
        ${pkgs.lib.getExe commitMessageLinter} "$message_path"
      }

      while ! lint_message; do
        if [[ ! -t 0 || ! -t 1 ]]; then
          printf 'correct the preserved message and retry: git commit --edit --file %q\n' \
            "$message_path" >&2
          exit 1
        fi
        editor=$(git var GIT_EDITOR)
        sh -c "$editor \"\$1\"" sh "$message_path"
      done

      if [[ -x $local_hook && \
        $(readlink -f "$local_hook") != $(readlink -f "$0") ]]; then
        exec "$local_hook" "$message_path"
      fi
    '';
  }
