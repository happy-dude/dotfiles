{
  homes,
  pkgs,
  self,
}: let
  inherit (homes) schan stachan;
  schanAgentShellConfig = schan.config.xdg.configFile."emacs/agent-shell.el".source;
  stachanAgentShellConfig = stachan.config.xdg.configFile."emacs/agent-shell.el".source;
  schanLspConfig = schan.config.xdg.configFile."emacs/lsp-servers.el".source;
  stachanLspConfig = stachan.config.xdg.configFile."emacs/lsp-servers.el".source;
  syntaxCheck =
    pkgs.runCommand "dotfiles-emacs-checks"
    {
      nativeBuildInputs = [
        pkgs.emacs-nox
        pkgs.findutils
      ];
    }
    ''
      while IFS= read -r -d ''' file; do
        emacs --batch --quick "$file" --eval '(check-parens)'
      done < <(find ${self} -type f -name '*.el' -print0)

      while IFS= read -r -d ''' file; do
        emacs --batch --quick "$file" \
          --eval "(require 'org-lint)" \
          --eval '(let ((reports (org-lint))) (when reports (error "%s: %S" buffer-file-name reports)))'
      done < <(find ${self} -type f -name '*.org' -print0)

      touch "$out"
    '';

  runtimeTest = pkgs.writeText "dotfiles-emacs-runtime-test.el" ''
    (require 'cl-lib)

    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "unexpected network access")))
              ((symbol-function 'package-refresh-contents)
               (lambda (&rest _) (error "unexpected package refresh")))
              ((symbol-function 'package-install)
               (lambda (&rest _) (error "unexpected package install")))
              ((symbol-function 'treesit-install-language-grammar)
               (lambda (&rest _) (error "unexpected grammar install")))
              ((symbol-function 'make-process)
               (lambda (&rest _) (error "unexpected process start")))
              ((symbol-function 'start-process)
               (lambda (&rest _) (error "unexpected process start"))))
      (load (getenv "DOTFILES_LSP_CONFIG") nil nil t)
      (require 'acp)
      (require 'shell-maker)
      (load (getenv "DOTFILES_AGENT_SHELL_CONFIG") nil nil t)
      (require 'agent-shell-opencode))

    (unless (null lsp-client-packages)
      (error "lsp-mode client packages may register downloaders: %S" lsp-client-packages))
    (when lsp-enable-suggest-server-download
      (error "lsp-mode server download suggestions are enabled"))
    (when treesit-auto-install
      (error "Tree-sitter grammar downloads are enabled"))

    (dolist (entry dotfiles-lsp-server-commands)
      (let* ((server-id (car entry))
             (command (cdr entry))
             (executable (car command))
             (client (gethash server-id lsp-clients)))
        (unless (and (string-prefix-p "/nix/store/" executable)
                     (file-executable-p executable))
          (error "%s is not a Nix-store executable: %S" server-id executable))
        (unless client
          (error "Missing registered LSP client: %s" server-id))
        (when (lsp--client-download-server-fn client)
          (error "%s retained a server downloader" server-id))))

    (unless (equal (length dotfiles-lsp-client-ids) 26)
      (error "Unexpected LSP client count: %s" (length dotfiles-lsp-client-ids)))
    (unless (and (string-prefix-p "/nix/store/" dotfiles-typescript-sdk)
                 (file-readable-p dotfiles-typescript-sdk))
      (error "TypeScript SDK is not pinned in the Nix store: %S" dotfiles-typescript-sdk))

    (dolist (language '(bash c clojure cpp css fennel fish go gomod haskell hcl
                             html javascript json kotlin latex lua markdown
                             markdown-inline nix perl python ruby rust sql
                             typescript typst vim yaml zig))
      (unless (treesit-language-available-p language)
        (error "Missing Nix-provided Tree-sitter grammar: %s" language)))

    (dolist (mode dotfiles-lsp-modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (unless (memq #'dotfiles/lsp-mode-setup (symbol-value hook))
          (error "Missing lsp-mode hook: %s" hook))))

    (unless (equal agent-shell-preferred-agent-config 'opencode)
      (error "OpenCode is not the preferred ACP agent"))
    (unless (equal agent-shell-opencode-acp-command '("opencode" "acp"))
      (error "Unexpected OpenCode ACP command: %S" agent-shell-opencode-acp-command))
    (unless (commandp 'agent-shell-opencode-start-agent)
      (error "OpenCode agent-shell command is unavailable"))
    (let ((opencode (executable-find "opencode")))
      (unless (and opencode (string-prefix-p "/nix/store/" opencode))
        (error "Emacs cannot find the Nix-managed OpenCode executable: %S" opencode)))
    (unless (equal (getenv "OPENCODE_DISABLE_LSP_DOWNLOAD") "true")
      (error "OpenCode's language-server download guard is missing"))
  '';

  runtimeCheck = home: let
    emacs = home.config.programs.emacs.finalPackage;
    agentShellConfig = home.config.xdg.configFile."emacs/agent-shell.el".source;
    homePath = home.config.home.path;
    lspConfig = home.config.xdg.configFile."emacs/lsp-servers.el".source;
  in
    pkgs.runCommand "dotfiles-emacs-runtime"
    {
      nativeBuildInputs = [emacs homePath];
    }
    ''
      export HOME="$TMPDIR/home"
      export XDG_CACHE_HOME="$HOME/.cache"
      export XDG_CONFIG_HOME="$HOME/.config"
      export XDG_DATA_HOME="$HOME/.local/share"
      export DOTFILES_AGENT_SHELL_CONFIG=${agentShellConfig}
      export DOTFILES_LSP_CONFIG=${lspConfig}
      export OPENCODE_DISABLE_LSP_DOWNLOAD=${home.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD}
      mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

      emacs --batch --quick --load ${runtimeTest}

      touch "$out"
    '';
in
  assert schanAgentShellConfig == stachanAgentShellConfig;
  assert schanLspConfig == stachanLspConfig;
  assert schan.config.programs.emacs.finalPackage == stachan.config.programs.emacs.finalPackage;
  assert schan.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true";
  assert stachan.config.home.sessionVariables.OPENCODE_DISABLE_LSP_DOWNLOAD == "true"; {
    emacs = syntaxCheck;
    emacs-runtime = runtimeCheck stachan;
  }
