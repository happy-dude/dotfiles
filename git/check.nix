{pkgs}: let
  commitMsgHook = import ./commit-msg-hook.nix {inherit pkgs;};
in {
  git-commit-message =
    pkgs.runCommand "git-commit-message-test"
    {
      nativeBuildInputs = [
        commitMsgHook
        pkgs.git
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

      cat >human.md <<'EOF'
      Human message with repository-specific style
      EOF
      commit-msg human.md

      touch "$out"
    '';
}
