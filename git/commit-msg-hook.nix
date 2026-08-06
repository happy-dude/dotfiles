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
      pkgs.gawk
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
        local cleaned_message
        local lint_status
        local normalized_message
        local raw_errors

        if [[ $agent_assisted == true ]] &&
          ! grep -q '^Assisted-by:' "$message_path"; then
          printf '%s\n' \
            "agent-assisted messages must retain an Assisted-by trailer" >&2
          return 1
        fi

        cleaned_message=$(mktemp)
        normalized_message=$(mktemp)
        raw_errors=$(mktemp)

        if grep -Fq -- \
          '------------------------ >8 ------------------------' \
          "$message_path"; then
          if ! awk \
            'index($0, "------------------------ >8 ------------------------") { exit } { print }' \
            "$message_path" | git stripspace --strip-comments \
            >"$cleaned_message"; then
            rm -f -- "$cleaned_message" "$normalized_message" "$raw_errors"
            return 1
          fi
        elif ${pkgs.lib.getExe commitMessageLinter} "$message_path" \
          >"$raw_errors" 2>&1; then
          rm -f -- "$cleaned_message" "$normalized_message" "$raw_errors"
          return 0
        else
          lint_status=$?
          if ! git stripspace <"$message_path" >"$normalized_message" ||
            ! git stripspace --strip-comments \
              <"$message_path" >"$cleaned_message"; then
            cat "$raw_errors" >&2
            rm -f -- "$cleaned_message" "$normalized_message" "$raw_errors"
            return "$lint_status"
          fi
          if cmp -s "$normalized_message" "$cleaned_message"; then
            cat "$raw_errors" >&2
            rm -f -- "$cleaned_message" "$normalized_message" "$raw_errors"
            return "$lint_status"
          fi
        fi

        if ${pkgs.lib.getExe commitMessageLinter} "$cleaned_message"; then
          lint_status=0
        else
          lint_status=$?
        fi
        rm -f -- "$cleaned_message" "$normalized_message" "$raw_errors"
        return "$lint_status"
      }

      if [[ -x $local_hook && \
        $(readlink -f "$local_hook") != $(readlink -f "$0") ]]; then
        "$local_hook" "$message_path"
      fi

      while ! lint_message; do
        if [[ ! -t 0 || ! -t 1 ]]; then
          printf 'correct the preserved message and retry:\ngit commit --amend --edit --file %q\n' \
            "$message_path" >&2
          exit 1
        fi
        editor=$(git var GIT_EDITOR)
        sh -c "$editor \"\$1\"" sh "$message_path"
      done
    '';
  }
