{
  homes,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (homes) schan stachan;
  schanAgentShellConfig = schan.config.xdg.configFile."emacs/agent-shell.el".source;
  stachanAgentShellConfig = stachan.config.xdg.configFile."emacs/agent-shell.el".source;
  schanLspConfig = schan.config.xdg.configFile."emacs/lsp-servers.el".source;
  schanLspPaths = schan.config.xdg.configFile."emacs/lsp-paths.el".source;
  stachanLspConfig = stachan.config.xdg.configFile."emacs/lsp-servers.el".source;
  stachanLspPaths = stachan.config.xdg.configFile."emacs/lsp-paths.el".source;
  opencode = import ../opencode/package.nix {inherit pkgs;};
  syntaxCheck = mkCheck {
    name = "dotfiles-emacs-checks";
    tools = [
      pkgs.emacs-nox
      pkgs.findutils
    ];
    script = ''
      while IFS= read -r -d ''' file; do
        emacs --batch --quick "$file" --eval '(check-parens)'
      done < <(find ${self} -type f -name '*.el' -print0)

      while IFS= read -r -d ''' file; do
        emacs --batch --quick "$file" \
          --eval "(require 'org-lint)" \
          --eval '(let ((reports (org-lint))) (when reports (error "%s: %S" buffer-file-name reports)))'
      done < <(find ${self} -type f -name '*.org' -print0)
    '';
  };

  runtimeTest = ./runtime-test.el;

  runtimeCheck = home: let
    emacs = home.config.programs.emacs.finalPackage;
    agentShellConfig = home.config.xdg.configFile."emacs/agent-shell.el".source;
    lspConfig = home.config.xdg.configFile."emacs/lsp-servers.el".source;
    lspPaths = home.config.xdg.configFile."emacs/lsp-paths.el".source;
  in
    mkCheck {
      name = "dotfiles-emacs-runtime";
      tools = [emacs opencode];
      script = ''
        export HOME="$TMPDIR/home"
        export XDG_CACHE_HOME="$HOME/.cache"
        export XDG_CONFIG_HOME="$HOME/.config"
        export XDG_DATA_HOME="$HOME/.local/share"
        export DOTFILES_AGENT_SHELL_CONFIG=${agentShellConfig}
        export DOTFILES_LSP_CONFIG=${lspConfig}
        export DOTFILES_LSP_PATHS=${lspPaths}
        export OPENCODE_DISABLE_LSP_DOWNLOAD=${home.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD}
        mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

        emacs --batch --quick --load ${runtimeTest}
      '';
    };
in
  assert schanAgentShellConfig == stachanAgentShellConfig;
  assert schanLspConfig == stachanLspConfig;
  assert schanLspPaths == stachanLspPaths;
  assert schan.config.programs.emacs.finalPackage == stachan.config.programs.emacs.finalPackage;
  assert schan.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true";
  assert stachan.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true"; {
    emacs = syntaxCheck;
    emacs-runtime = runtimeCheck stachan;
  }
