{
  homes,
  lib,
  pkgs,
  self,
}: let
  mkCheck = import ../lib/mkCheck.nix {inherit pkgs;};
  inherit (import ../lib/homes.nix {inherit lib;}) shared;
  agentShellConfig = shared homes "the emacs agent-shell configuration" (
    home: home.config.xdg.configFile."emacs/agent-shell.el".source
  );
  lspConfig = shared homes "the emacs lsp-servers configuration" (
    home: home.config.xdg.configFile."emacs/lsp-servers.el".source
  );
  lspPaths = shared homes "the emacs lsp-paths configuration" (
    home: home.config.xdg.configFile."emacs/lsp-paths.el".source
  );
  emacsPackage = shared homes "the emacs package" (
    home: home.config.programs.emacs.finalPackage
  );
  opencodeDisableLspDownload = shared homes "OPENCODE_DISABLE_LSP_DOWNLOAD" (
    home: home.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD
  );
  initEl = shared homes "the emacs init.el" (
    home: home.config.xdg.configFile."emacs/init.el".source
  );
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
in
  assert opencodeDisableLspDownload == "true";
  assert lib.all (home: lib.elem opencode home.config.home.packages) (
    lib.attrValues homes
  ); {
    emacs = syntaxCheck;
    emacs-runtime = mkCheck {
      name = "dotfiles-emacs-runtime";
      tools = [emacsPackage opencode];
      script = ''
        export HOME="$TMPDIR/home"
        export XDG_CACHE_HOME="$HOME/.cache"
        export XDG_CONFIG_HOME="$HOME/.config"
        export XDG_DATA_HOME="$HOME/.local/share"
        export DOTFILES_AGENT_SHELL_CONFIG=${agentShellConfig}
        export DOTFILES_LSP_CONFIG=${lspConfig}
        export DOTFILES_LSP_PATHS=${lspPaths}
        export OPENCODE_DISABLE_LSP_DOWNLOAD=${opencodeDisableLspDownload}
        mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

        # init.el locates its companions through the user emacs directory.
        mkdir -p "$XDG_CONFIG_HOME/emacs"
        ln -s ${lspPaths} "$XDG_CONFIG_HOME/emacs/lsp-paths.el"
        ln -s ${lspConfig} "$XDG_CONFIG_HOME/emacs/lsp-servers.el"
        ln -s ${agentShellConfig} "$XDG_CONFIG_HOME/emacs/agent-shell.el"

        emacs --batch --quick --load ${runtimeTest}
        emacs --batch --quick --load ${initEl} \
          --eval '(unless (dotfiles/sensitive-file-p (expand-file-name "~/.config/opencode/local.json")) (error "opencode path is not guarded"))'
      '';
    };
  }
