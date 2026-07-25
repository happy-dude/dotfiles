;;; runtime-test.el --- Assert the evaluated Emacs runtime -*- lexical-binding: t; -*-

;; Loaded by the emacs-runtime flake check against a real Emacs built from
;; this configuration. Paths come from the environment so that the file
;; stays ordinary Emacs Lisp.

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
  (load (getenv "DOTFILES_LSP_PATHS") nil nil t)
  (load (getenv "DOTFILES_LSP_CONFIG") nil nil t)
  (require 'acp)
  (require 'shell-maker)
  (load (getenv "DOTFILES_AGENT_SHELL_CONFIG") nil nil t)
  (require 'agent-shell-opencode))

(unless (null lsp-client-packages)
  (error "lsp-mode client packages may register downloaders: %S" lsp-client-packages))
(when lsp-enable-suggest-server-download
  (error "lsp-mode server download suggestions are enabled"))
(when lsp-enable-snippet
  (error "lsp-mode snippets require the undeclared yasnippet package"))
(when treesit-auto-install
  (error "Tree-sitter grammar downloads are enabled"))
(unless (equal (cdr (assq 'lsp-mode minor-mode-alist)) '(" LSP"))
  (error "lsp-mode retained a nested mode-line indicator"))

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

(let ((lsp-mode t)
      (lsp-managed-mode nil)
      (format-calls 0))
  (cl-letf (((symbol-function 'lsp-feature?) (lambda (_) t))
            ((symbol-function 'lsp-format-buffer)
             (lambda () (setq format-calls (1+ format-calls)))))
    (dotfiles/lsp-format-buffer-if-supported))
  (unless (zerop format-calls)
    (error "Formatter ran before LSP managed the document")))

(let ((lsp-managed-mode t)
      (format-calls 0))
  (cl-letf (((symbol-function 'lsp-feature?) (lambda (_) t))
            ((symbol-function 'lsp-format-buffer)
             (lambda () (setq format-calls (1+ format-calls)))))
    (dotfiles/lsp-format-buffer-if-supported))
  (unless (= format-calls 1)
    (error "Formatter did not run for a managed document")))

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

;;; runtime-test.el ends here
