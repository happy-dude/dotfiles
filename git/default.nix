{
  config,
  lib,
  pkgs,
  ...
}: let
  commitMsgHook = import ./commit-msg-hook.nix {inherit pkgs;};
in {
  # Per-machine identity + signing (user.email, user.signingkey, commit/tag
  # gpgsign) live in an untracked ~/.config/git/local.config, included below —
  # keys and email differ per box (git/local.config.example is the template).
  #
  # Home Manager writes ~/.config/git/config, which an unmanaged ~/.gitconfig
  # silently overrides (git reads it last).
  programs.git = {
    enable = true;

    # Global gitignore -> ~/.config/git/ignore (git reads it by default; no
    # core.excludesFile needed). Kept as a plain file, read in.
    ignores = lib.splitString "\n" (builtins.readFile ./.gitignore_global);

    includes = [
      {path = "${config.home.homeDirectory}/.config/git/local.config";}
    ];

    settings = {
      user.name = "Stanley Chan";

      alias = {
        c = "!git --no-pager show --no-patch --format='commit %h (\"%s\")%n'";
        hist = "log --date=relative --abbrev=12 -n 160 --pretty='format:%C(dim blue)%h%C(auto)%d %s %>|(68,trunc)%C(8)- %C(dim magenta)%an%C(8), %ad' --graph --all";
        out = "log @{u}..";
        type = "cat-file -t";
        dump = "cat-file -p";
        dft = "difftool";
        # Lists all local branches that have been deleted on remote
        gone = "!git for-each-ref --format '%(refname:short) %(upstream:track)' | awk '$2 == \"[gone]\" {print $1}' | xargs -r git branch -D";
        alias = "! git config --get-regexp ^alias\\. | sed -e s/^alias\\.// -e s/\\ /\\ =\\ /";
      };

      core = {
        # Recursive operations otherwise start a detached daemon for each
        # initialized submodule and exhaust the per-user inotify instances.
        fsmonitor = false;
        untrackedCache = true;
        pager = "bat";
        hooksPath = "${config.xdg.configHome}/git/hooks";
      };
      diff = {
        algorithm = "histogram";
        colorMoved = "plain";
        indentHeuristic = true;
        mnemonicPrefix = true;
        renames = true;
      };
      init.defaultBranch = "main";
      pull.rebase = true;
      log.date = "local";
      apply.whitespace = "fix";
      column.ui = "auto";
      branch.sort = "-committerdate";
      tag.sort = "version:refname";
      push = {
        default = "simple";
        autoSetupRemote = true;
        followTags = true;
      };
      fetch = {
        prune = true;
        pruneTags = true;
        all = true;
      };
      help.autocorrect = "prompt";
      commit.verbose = true;
      rerere = {
        enabled = true;
        autoupdate = true;
      };
      rebase = {
        autoSquash = true;
        autoStash = true;
        updateRefs = true;
      };
      merge.conflictStyle = "zdiff3"; # (use plain 'diff3' if git < 2.35)
      url."git@github.com:".insteadOf = "https://github.com/";
    };
  };

  xdg.configFile."git/hooks/commit-msg".source = pkgs.lib.getExe commitMsgHook;

  # delta (enableGitIntegration) sets per-command [pager] + interactive.diffFilter,
  # not core.pager, so it coexists with core.pager = bat above.
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      navigate = true; # n / N to jump between diff sections
      features = "gruvmax-fang";

      gruvmax-fang = {
        # General appearance
        syntax-theme = "gruvbox-dark";
        # File
        file-style = "#FFFFFF bold";
        file-added-label = "[+]";
        file-copied-label = "[==]";
        file-modified-label = "[*]";
        file-removed-label = "[-]";
        file-renamed-label = "[->]";
        file-decoration-style = "#84786A ul";
        # Hunk headers
        hunk-header-style = "omit";
        # Line numbers
        line-numbers = true;
        line-numbers-left-style = "#84786A";
        line-numbers-right-style = "#84786A";
        line-numbers-minus-style = "#A02A11";
        line-numbers-plus-style = "#479B36";
        line-numbers-zero-style = "#84786A";
        line-numbers-left-format = " {nm:>3} │";
        line-numbers-right-format = " {np:>3} │";
        # Diff contents
        inline-hint-style = "syntax";
        minus-style = "syntax #330011";
        minus-emph-style = "syntax #80002a";
        minus-non-emph-style = "syntax auto";
        plus-style = "syntax #001a00";
        plus-emph-style = "syntax #003300";
        plus-non-emph-style = "syntax auto";
        whitespace-error-style = "#FB4934 reverse";
        # Commit hash
        commit-decoration-style = "normal box";
        commit-style = "#ffffff bold";
        # Blame
        blame-code-style = "syntax";
        blame-format = "{author:>18} ({commit:>8}) {timestamp:<13} ";
        blame-palette = "#000000 #1d2021 #282828 #3c3836";
        # Merge conflicts
        merge-conflict-begin-symbol = "⌃";
        merge-conflict-end-symbol = "⌄";
        merge-conflict-ours-diff-header-style = "#FABD2F bold";
        merge-conflict-theirs-diff-header-style = "#FABD2F bold overline";
        merge-conflict-ours-diff-header-decoration-style = "";
        merge-conflict-theirs-diff-header-decoration-style = "";
      };
    };
  };
}
