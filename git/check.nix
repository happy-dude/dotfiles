{pkgs}: let
  commitMsgHook = import ./commit-msg-hook.nix {inherit pkgs;};
in {
  git-commit-message =
    pkgs.runCommand "git-commit-message-test"
    {
      nativeBuildInputs = [
        commitMsgHook
        pkgs.git
        pkgs.util-linux
      ];
    }
    ''
      mkdir repo
      cd repo
      git init --quiet
      cat >.git/hooks/commit-msg <<'EOF'
      #!${pkgs.bash}/bin/bash
      if [[ ''${1##*/} == post-local-rewrite.md ]]; then
        printf '%s\n' 'Local hook replaced the validated message' >"$1"
      fi
      touch local-hook-ran
      EOF
      chmod 0755 .git/hooks/commit-msg

      cat >valid.md <<'EOF'
      git: validate agent-assisted messages

      Enforce the shared commit-message policy without replacing local hooks.

      Assisted-by: ChatGPT (gpt-5.6-sol, medium, OpenCode)
      EOF
      commit-msg valid.md
      test -e local-hook-ran

      cp valid.md post-local-rewrite.md
      if commit-msg post-local-rewrite.md; then
        echo "accepted a message invalidated by the repository-local hook" >&2
        exit 1
      fi

      cat >invalid-agent.md <<'EOF'
      Invalid agent subject

      Assisted-by: ChatGPT (gpt-5.6-sol, medium, OpenCode)
      EOF
      if commit-msg invalid-agent.md; then
        echo "accepted an invalid agent-assisted message" >&2
        exit 1
      fi

      cat >editor <<'EOF'
      #!${pkgs.bash}/bin/bash
      cat >"$1" <<'MESSAGE'
      git: repair an invalid interactive message

      Reopen the preserved message and continue only after validation succeeds.

      Assisted-by: ChatGPT (gpt-5.6-sol, medium, OpenCode)
      MESSAGE
      EOF
      chmod 0755 editor
      GIT_EDITOR="$PWD/editor" \
        script --quiet --return --command \
          "commit-msg invalid-agent.md" /dev/null
      grep -Fx 'git: repair an invalid interactive message' invalid-agent.md

      cat >human.md <<'EOF'
      human: use repository-specific style
      EOF
      commit-msg human.md

      cat >invalid-human.md <<'EOF'
      Human message without a subsystem
      EOF
      if commit-msg invalid-human.md; then
        echo "accepted an invalid human-authored message" >&2
        exit 1
      fi

      touch "$out"
    '';
}
