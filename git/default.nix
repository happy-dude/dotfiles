{
  config,
  lib,
  ...
}:

{
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
      { path = "${config.home.homeDirectory}/.config/git/local.config"; }
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
        fsmonitor = true;
        untrackedCache = true;
        pager = "bat";
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

  # delta (enableGitIntegration) sets per-command [pager] + interactive.diffFilter,
  # not core.pager, so it coexists with core.pager = bat above.
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      navigate = true; # n / N to jump between diff sections
      line-numbers = true;
      syntax-theme = "gruvbox-dark";
    };
  };
}
