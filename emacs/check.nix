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
  rainbowPalette = shared homes "the emacs rainbow-delimiters palette" (
    home: home.config.xdg.configFile."emacs/rainbow-delimiters-palette.el".source
  );
  rainbowColours = pkgs.writeText "rainbow-delimiters.json" (
    builtins.toJSON (import ../lib/rainbow-delimiters.nix)
  );
  sensitivePaths = shared homes "the emacs sensitive-paths list" (
    home: home.config.xdg.configFile."emacs/sensitive-paths.el".source
  );
  sensitivePathList = pkgs.writeText "sensitive-paths.json" (
    builtins.toJSON (import ../lib/sensitive-paths.nix)
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
  assert lib.all (home: lib.elem "writeBoundary" home.config.home.activation.checkEmacsInitPaths.before) (lib.attrValues homes);
  assert lib.all (
    home:
      lib.hasPrefix "${home.config.programs.emacs.finalPackage}/bin/emacsclient "
      home.config.xdg.desktopEntries.emacs-org-protocol.exec
  ) (lib.attrValues homes);
  assert lib.all (home: lib.elem opencode home.config.home.packages) (
    lib.attrValues homes
  ); {
    emacs = syntaxCheck;
    emacs-org-bookmarklet = mkCheck {
      name = "dotfiles-emacs-org-bookmarklet";
      tools = [pkgs.nodejs];
      script = ''
        DOTFILES_ORG_PROTOCOL_DOC=${../docs/emacs-org-protocol.md} \
          node --test ${./bookmarklet-test.mjs}
      '';
    };
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
        export DOTFILES_RAINBOW_PALETTE=${rainbowColours}
        export DOTFILES_SENSITIVE_PATHS=${sensitivePathList}
        export OPENCODE_DISABLE_LSP_DOWNLOAD=${opencodeDisableLspDownload}
        mkdir -p "$XDG_CACHE_HOME" "$XDG_DATA_HOME"

        # init.el locates its companions through the user emacs directory,
        # which follows XDG_CONFIG_HOME.
        link_companions() {
          mkdir -p "$XDG_CONFIG_HOME/emacs"
          ln -s ${initEl} "$XDG_CONFIG_HOME/emacs/init.el"
          ln -s ${lspPaths} "$XDG_CONFIG_HOME/emacs/lsp-paths.el"
          ln -s ${lspConfig} "$XDG_CONFIG_HOME/emacs/lsp-servers.el"
          ln -s ${agentShellConfig} "$XDG_CONFIG_HOME/emacs/agent-shell.el"
          ln -s ${rainbowPalette} "$XDG_CONFIG_HOME/emacs/rainbow-delimiters-palette.el"
          ln -s ${sensitivePaths} "$XDG_CONFIG_HOME/emacs/sensitive-paths.el"
        }
        link_companions

        # A cache alone can hide the XDG init. Preflight must fail before writing,
        # it must not move user-owned content itself.
        check_init() {
          bash ${./check-init-paths.sh} "$XDG_CONFIG_HOME/emacs" "$TMPDIR/old-generation"
        }
        refuses_init() {
          if check_init >"$TMPDIR/preflight.log" 2>&1; then
            echo "unexpected Emacs preflight success for $1" >&2
            exit 1
          fi
          grep -F -- "$1" "$TMPDIR/preflight.log"
        }
        mkdir -p "$HOME/.emacs.d" "$TMPDIR/old-generation" "$TMPDIR/old-files"
        printf 'keep\n' >"$HOME/.emacs.d/cache"
        refuses_init "$HOME/.emacs.d"
        grep -Fx keep "$HOME/.emacs.d/cache"
        mv "$HOME/.emacs.d" "$HOME/legacy-emacs-backup"
        check_init
        ln -s "$TMPDIR/old-files" "$TMPDIR/old-generation/home-files"
        ln -s ${initEl} "$TMPDIR/old-files/.emacs"
        ln -s "$TMPDIR/old-files/.emacs" "$HOME/.emacs"
        check_init
        test -L "$HOME/.emacs"
        # A foreign link to the exact same store file is not the retired link.
        ln -sfn ${initEl} "$HOME/.emacs"
        refuses_init "$HOME/.emacs"
        test -L "$HOME/.emacs"
        rm "$HOME/.emacs"
        ln -s "$XDG_CONFIG_HOME/emacs" "$HOME/.emacs.d"
        check_init
        ln -s .emacs.d/init.el "$HOME/.emacs"
        check_init
        rm "$HOME/.emacs"
        touch "$HOME/.emacs.el"
        refuses_init "$HOME/.emacs.el"
        rm "$HOME/.emacs.el"
        rm "$HOME/.emacs.d"
        mkdir -p "$HOME/org/roam" "$TMPDIR/run"
        chmod 0700 "$TMPDIR/run"
        printf '(defvar dotfiles-shadow-loaded t)\n' >"$HOME/.emacs.el"
        emacs -Q --batch -f batch-byte-compile "$HOME/.emacs.el"
        # Suffixes alone do not select the home-root init on Linux. In the
        # XDG directory, however, the same bytecode really does win.
        cp "$HOME/.emacs.elc" "$XDG_CONFIG_HOME/emacs/init.elc"
        refuses_init "$XDG_CONFIG_HOME/emacs/init.elc"
        XDG_RUNTIME_DIR="$TMPDIR/run" timeout 60 emacs --fg-daemon=compiled-check \
          --eval '(kill-emacs (if (bound-and-true-p dotfiles-shadow-loaded) 0 1))'
        rm "$XDG_CONFIG_HOME/emacs/init.elc"
        check_init
        # Start normally after the user has moved the shadowing directory.
        # --batch --load would succeed even if Emacs ignored the deployed init.
        XDG_RUNTIME_DIR="$TMPDIR/run" timeout 60 emacs --fg-daemon=xdg-check \
          --eval '(kill-emacs (if (and user-init-file (not init-file-had-error)
                                    (not (bound-and-true-p dotfiles-shadow-loaded))
                                    (file-equal-p user-init-file (expand-file-name "emacs/init.el" (getenv "XDG_CONFIG_HOME")))
                                    (memq (quote dotfiles/harden-sensitive-buffer) find-file-hook)) 0 1))'

        emacs --batch --quick --load ${runtimeTest}
        emacs --batch --quick --load ${initEl} \
          --load ${./org-protocol-test.el}

        # Old credentials can remain under the standard roots after XDG
        # relocation. Cover both roots and a credential directory symlink.
        export XDG_CONFIG_HOME="$HOME/relocated-config"
        export XDG_DATA_HOME="$HOME/relocated-data"
        mkdir -p "$HOME/private/keys"
        ln -s "$HOME/private/keys" "$HOME/.ssh"
        link_companions
        emacs --batch --quick --load ${initEl} \
          --load ${./secret-state-test.el}
      '';
    };
  }
