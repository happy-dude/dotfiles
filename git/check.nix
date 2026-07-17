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
      Human message with repository-specific style
      EOF
      commit-msg human.md

      touch "$out"
    '';
}
