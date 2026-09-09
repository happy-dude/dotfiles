{pkgs}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  commitMsgHook = import ./commit-msg-hook.nix {inherit pkgs;};
  localHook = import ./local-hook.nix {inherit pkgs;};
in {
  git-local-hook = mkCheck {
    name = "git-local-hook-test";
    tools = [pkgs.git];
    script = ''
      mkdir hooks repo
      # Installed the way Home Manager does: one symlink per hook name.
      for name in pre-commit post-commit pre-push; do
        ln -s ${pkgs.lib.getExe localHook} "hooks/$name"
      done
      cd repo
      git init --quiet
      git config user.name test
      git config user.email test@example.invalid
      git config core.hooksPath "$PWD/../hooks"

      # No repository hook: the dispatcher is a no-op and the commit proceeds.
      git commit --quiet --allow-empty -m 'test: no local hook'

      # A repository hook runs with the dispatcher's arguments and its exit
      # status decides the outcome.
      cat >.git/hooks/pre-commit <<'EOF'
      #!${pkgs.bash}/bin/bash
      touch "$(git rev-parse --show-toplevel)/pre-commit-ran"
      exit "$(cat "$(git rev-parse --show-toplevel)/pre-commit-status")"
      EOF
      chmod 0755 .git/hooks/pre-commit
      printf '0\n' >pre-commit-status
      git commit --quiet --allow-empty -m 'test: local hook accepts'
      test -e pre-commit-ran

      rm pre-commit-ran
      printf '1\n' >pre-commit-status
      if git commit --quiet --allow-empty -m 'test: local hook rejects'; then
        echo "committed although the repository hook rejected" >&2
        exit 1
      fi
      test -e pre-commit-ran
    '';
  };

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
      if grep -q 'local-hook-reject' "$1"; then
        exit 1
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

      # The marker inside prose is not a scissors line; the text after it
      # is part of the message and must still be checked.
      cat >quoted-scissors.md <<'EOF'
      git: mention the scissors marker in prose

      Git separates the diff with ------------------------ >8 ------------------------ in verbose mode.

      This deliberately overlong line after the mention must still reach the width check.
      EOF
      if commit-msg quoted-scissors.md </dev/null; then
        echo "accepted overlong text hidden behind a quoted scissors marker" >&2
        exit 1
      fi

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
      # Git hands hooks /dev/null on stdin; only the terminal on stderr and
      # /dev/tty distinguish an interactive commit.
      GIT_EDITOR="$PWD/editor" \
        script --quiet --return --command \
          "commit-msg invalid-agent.md </dev/null" /dev/null
      grep -Fx 'git: repair an invalid interactive message' invalid-agent.md

      cat >unchanged.md <<'EOF'
      Invalid agent subject

      Assisted-by: ChatGPT (gpt-5.6-sol, medium, OpenCode)
      EOF
      cat >noop-editor <<'EOF'
      #!${pkgs.bash}/bin/bash
      exit 0
      EOF
      chmod 0755 noop-editor
      if GIT_EDITOR="$PWD/noop-editor" script --quiet --return --command \
        "commit-msg unchanged.md </dev/null" /dev/null; then
        echo "accepted a message the editor left unchanged" >&2
        exit 1
      fi

      # git commit -m in a terminal: Git exports GIT_EDITOR=: and the hook
      # must print the recovery hint rather than run ':' as the editor.
      cp unchanged.md no-editor.md
      if GIT_EDITOR=: script --quiet --return --command \
        "commit-msg no-editor.md </dev/null" no-editor.txt; then
        echo "accepted an invalid message when git had no editor" >&2
        exit 1
      fi
      grep -F -- 'git commit --edit --file no-editor.md' no-editor.txt

      # An edit the repository-local hook rejects must not slip past it
      # merely because the global linter accepts the new text.
      cat >rejected-editor <<'EOF'
      #!${pkgs.bash}/bin/bash
      cat >"$1" <<'MESSAGE'
      git: introduce text the local hook rejects

      local-hook-reject

      Assisted-by: ChatGPT (gpt-5.6-sol, medium, OpenCode)
      MESSAGE
      EOF
      chmod 0755 rejected-editor
      cp unchanged.md local-reject.md
      if GIT_EDITOR="$PWD/rejected-editor" script --quiet --return --command \
        "commit-msg local-reject.md </dev/null" /dev/null; then
        echo "accepted an edit the repository-local hook rejects" >&2
        exit 1
      fi

      cat >failing-editor <<'EOF'
      #!${pkgs.bash}/bin/bash
      exit 3
      EOF
      chmod 0755 failing-editor
      # stderr must stay on the terminal or the hook treats the commit as
      # headless; read the diagnostic from the pty transcript instead.
      cp unchanged.md editor-failed.md
      if GIT_EDITOR="$PWD/failing-editor" script --quiet --return --command \
        "commit-msg editor-failed.md </dev/null" editor-failed.txt; then
        echo "accepted a message after the editor failed" >&2
        exit 1
      fi
      grep -F 'editor failed' editor-failed.txt

      cp unchanged.md headless.md
      if commit-msg headless.md </dev/null 2>hint.txt; then
        echo "accepted an invalid message without a terminal" >&2
        exit 1
      fi
      grep -F -- 'git commit --edit --file headless.md' hint.txt
      if grep -F -- '--amend --edit' hint.txt; then
        echo "the hint still suggests --amend" >&2
        exit 1
      fi

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
