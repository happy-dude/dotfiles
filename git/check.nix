{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  commitMsgHook = import ./commit-msg-hook.nix {inherit pkgs;};
in {
  git-commit-message = mkCheck {
    name = "git-commit-message-test";
    tools = [
      commitMsgHook
      pkgs.git
      pkgs.util-linux
    ];
    script = ''
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

      generated_subjects=(
        "Merge branch 'main' into macos"
        'Revert "git: validate agent-assisted messages"'
        'Reapply "git: validate agent-assisted messages"'
        "fixup! git: validate agent-assisted messages"
        "squash! git: validate agent-assisted messages"
        "amend! git: validate agent-assisted messages"
        "Squashed commit of the following:"
      )
      for subject in "''${generated_subjects[@]}"; do
        printf '%s\n' "$subject" >generated.md
        commit-msg generated.md
      done

      cat >verbose.md <<'EOF'
      git: lint the cleaned verbose message

      Ignore Git's status comments and verbose diff during Markdown checks.

      # Please enter the commit message for your changes.
      # ------------------------ >8 ------------------------
      # Everything below it will be ignored.
      diff --git c/file i/file
      --- c/file
      +++ i/file
      @@ -0,0 +1 @@
      +This deliberately unformatted diff line must not reach Prettier or width checks.
      EOF
      commit-msg verbose.md

      cat >merge-template.md <<'EOF'
      Merge branch 'main' into macos

      Signed-off-by: Example User <user@example.invalid>
      # Please enter a commit message to explain why this merge is necessary,
      # especially if it merges an updated upstream into a topic branch.
      #
      # Lines starting with '#' will be ignored, and an empty message aborts
      # the commit.
      EOF
      commit-msg merge-template.md

      git config core.commentChar ';'
      cat >custom-comment-template.md <<'EOF'
      git: clean a custom comment template

      Keep the actual commit message.
      ; Please enter the commit message for your changes.
      ; Lines starting with ';' will be ignored.
      EOF
      commit-msg custom-comment-template.md
      git config --unset core.commentChar

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

    '';
  };
}
