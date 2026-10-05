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
      pkgs.diffutils
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

        # Git cuts only at its comment string, a space, and the marker on a
        # line of their own. The last core.commentChar or core.commentString
        # wins; "auto" can be any of Git's candidates; unset means '#'.
        comment_string=$(
          { git config --get-regexp '^core\.comment(char|string)$' || :; } |
            awk 'END { sub(/^[^ ]* /, ""); print }'
        )
        case ''${comment_string,,} in
        "") comment_strings=('#') ;;
        auto) comment_strings=('#' ';' '@' '!' '$' '%' '^' '&' '|' ':') ;;
        *) comment_strings=("$comment_string") ;;
        esac
        scissors_lines=()
        for comment_string in "''${comment_strings[@]}"; do
          scissors_lines+=(
            "$comment_string ------------------------ >8 ------------------------"
          )
        done
        if printf '%s\n' "''${scissors_lines[@]}" |
          grep -Fxqf - -- "$message_path"; then
          if ! printf '%s\n' "''${scissors_lines[@]}" |
            awk 'NR == FNR { cut[$0]; next } $0 in cut { exit } { print }' \
              - "$message_path" | git stripspace --strip-comments \
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

      run_local_hook() {
        if [[ -x $local_hook && \
          $(readlink -f "$local_hook") != $(readlink -f "$0") ]]; then
          "$local_hook" "$message_path"
        fi
      }

      # Git runs hooks with stdin on /dev/null and stdout joined to stderr, so
      # only stderr and the controlling terminal reveal an interactive commit.
      # The repository-local hook sees every edited message, not only the
      # first one.
      while ! { run_local_hook && lint_message; }; do
        # Git exports GIT_EDITOR=: when it will not open an editor itself
        # (-m, -F, --no-edit), so only a real editor allows re-editing.
        editor=$(git var GIT_EDITOR)
        if [[ ! -t 2 || $editor == : || $editor == true ]] ||
          ! { : </dev/tty; } 2>/dev/null; then
          printf '%s\n' \
            'correct the preserved message and retry:' \
            "git commit --edit --file $(printf '%q' "$message_path")" \
            '(add --amend only if the rejected commit was itself an amend)' >&2
          exit 1
        fi
        before=$(mktemp)
        cp -- "$message_path" "$before"
        if ! sh -c "$editor \"\$1\"" sh "$message_path" </dev/tty >/dev/tty; then
          rm -f -- "$before"
          printf '%s\n' 'editor failed; aborting the commit' >&2
          exit 1
        fi
        if cmp -s "$before" "$message_path"; then
          rm -f -- "$before"
          printf '%s\n' 'message unchanged; aborting the commit' >&2
          exit 1
        fi
        rm -f -- "$before"
      done
    '';
  }
