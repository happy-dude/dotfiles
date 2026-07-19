{
  homes,
  pkgs,
  self,
}: let
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
      (load (getenv "DOTFILES_LSP_CONFIG") nil nil t))

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
  '';

  runtimeCheck = name: home: let
    emacs = home.config.programs.emacs.finalPackage;
    lspConfig = home.config.xdg.configFile."emacs/lsp-servers.el".source;
  in
    pkgs.runCommand "dotfiles-emacs-runtime-${name}"
    {
      nativeBuildInputs = [emacs];
    }
    ''
      export HOME="$TMPDIR/home"
      export XDG_CACHE_HOME="$HOME/.cache"
      export XDG_CONFIG_HOME="$HOME/.config"
      export XDG_DATA_HOME="$HOME/.local/share"
      export DOTFILES_LSP_CONFIG=${lspConfig}
      mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

      emacs --batch --quick --load ${runtimeTest}

      touch "$out"
    '';
in
  {
    emacs = syntaxCheck;
  }
  // builtins.listToAttrs (map (name: {
    name = "emacs-runtime-${name}";
    value = runtimeCheck name homes.${name};
  }) (builtins.attrNames homes))
