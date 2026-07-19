{pkgs}: let
  perlWithLanguageServer = pkgs.perl.withPackages (ps: [ps.PerlLanguageServer]);
  treesitGrammars = pkgs.emacsPackages.treesit-grammars.with-grammars (grammars:
    with grammars; [
      tree-sitter-bash
      tree-sitter-c
      tree-sitter-clojure
      tree-sitter-cpp
      tree-sitter-css
      tree-sitter-fennel
      tree-sitter-fish
      tree-sitter-go
      tree-sitter-gomod
      tree-sitter-haskell
      tree-sitter-hcl
      tree-sitter-html
      tree-sitter-javascript
      tree-sitter-json
      tree-sitter-kotlin
      tree-sitter-latex
      tree-sitter-lua
      tree-sitter-markdown
      tree-sitter-markdown-inline
      tree-sitter-nix
      tree-sitter-perl
      tree-sitter-python
      tree-sitter-ruby
      tree-sitter-rust
      tree-sitter-sql
      # The locked TSX derivation exports tree_sitter_typescript instead of
      # tree_sitter_tsx, so Emacs cannot load it. web-mode remains the TSX mode.
      tree-sitter-typescript
      tree-sitter-typst
      tree-sitter-vim
      tree-sitter-yaml
      tree-sitter-zig
    ]);
in {
  packages = epkgs:
    with epkgs; [
      auctex
      clojure-mode
      company
      company-box
      fennel-mode
      fish-mode
      go-mode
      haskell-mode
      kotlin-mode
      lsp-mode
      lua-mode
      rust-mode
      terraform-mode
      treesit-auto
      typescript-mode
      typst-ts-mode
      vimrc-mode
      web-mode
      zig-mode
    ];

  config = pkgs.writeText "emacs-lsp-servers.el" ''
    ;;; lsp-servers.el --- Nix-owned LSP and Tree-sitter configuration -*- lexical-binding: t; -*-

    (require 'cl-lib)
    (require 'seq)

    ;; Nix owns every client and server.  Do not load lsp-mode's client modules,
    ;; dependency recipes, or downloader prompts implicitly.
    (setq lsp-client-packages nil
          lsp-enable-suggest-server-download nil
          lsp-auto-guess-root t
          lsp-completion-provider :capf)

    (require 'lsp-mode)
    (require 'lsp-completion)

    (setq treesit-extra-load-path '("${treesitGrammars}/lib")
          treesit-auto-install nil)
    (require 'treesit-auto)
    (global-treesit-auto-mode 1)

    (require 'company)
    (require 'company-box)
    (global-company-mode 1)
    (add-hook 'company-mode-hook #'company-box-mode)

    (defconst dotfiles-lsp-server-commands
      '((dotfiles-bash . ("${pkgs.bash-language-server}/bin/bash-language-server" "start"))
        (dotfiles-clangd . ("${pkgs.clang-tools}/bin/clangd"))
        (dotfiles-clojure . ("${pkgs.clojure-lsp}/bin/clojure-lsp" "listen"))
        (eslint . ("${pkgs.vscode-langservers-extracted}/bin/vscode-eslint-language-server" "--stdio"))
        (dotfiles-fennel . ("${pkgs.fennel-ls}/bin/fennel-ls" "--server"))
        (dotfiles-fish . ("${pkgs.fish-lsp}/bin/fish-lsp" "start"))
        (dotfiles-gopls . ("${pkgs.gopls}/bin/gopls"))
        (dotfiles-haskell . ("${pkgs.haskell-language-server}/bin/haskell-language-server-wrapper" "--lsp"))
        (dotfiles-json . ("${pkgs.vscode-langservers-extracted}/bin/vscode-json-language-server" "--stdio"))
        (dotfiles-kotlin . ("${pkgs.kotlin-language-server}/bin/kotlin-language-server"))
        (dotfiles-lua . ("${pkgs.lua-language-server}/bin/lua-language-server"))
        (dotfiles-marksman . ("${pkgs.marksman}/bin/marksman" "server"))
        (dotfiles-nixd . ("${pkgs.nixd}/bin/nixd"))
        (dotfiles-oxlint . ("${pkgs.oxlint}/bin/oxlint" "--lsp"))
        (dotfiles-perl-language-server . ("${perlWithLanguageServer}/bin/perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"))
        (dotfiles-perl-navigator . ("${pkgs.perlnavigator}/bin/perlnavigator" "--stdio"))
        (dotfiles-ruff . ("${pkgs.ruff}/bin/ruff" "server"))
        (dotfiles-rust-analyzer . ("${pkgs.rust-analyzer}/bin/rust-analyzer"))
        (dotfiles-terraform . ("${pkgs.terraform-ls}/bin/terraform-ls" "serve"))
        (dotfiles-texlab . ("${pkgs.texlab}/bin/texlab"))
        (dotfiles-tinymist . ("${pkgs.tinymist}/bin/tinymist" "lsp"))
        (dotfiles-typescript . ("${pkgs.typescript-language-server}/bin/typescript-language-server" "--stdio"))
        (dotfiles-vim . ("${pkgs.vim-language-server}/bin/vim-language-server" "--stdio"))
        (dotfiles-yaml . ("${pkgs.yaml-language-server}/bin/yaml-language-server" "--stdio"))
        (dotfiles-zls . ("${pkgs.zls}/bin/zls"))
        (dotfiles-zuban . ("${pkgs.zuban}/bin/zuban" "server"))))

    (defconst dotfiles-lsp-client-ids (mapcar #'car dotfiles-lsp-server-commands))

    (defun dotfiles/lsp-command (server-id)
      (or (alist-get server-id dotfiles-lsp-server-commands)
          (error "No Nix command registered for %s" server-id)))

    (cl-defun dotfiles/lsp-register (server-id major-modes
                                               &key add-on activation-fn
                                               initialization-options priority)
      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-stdio-connection (dotfiles/lsp-command server-id))
        :major-modes major-modes
        :activation-fn activation-fn
        :server-id server-id
        :priority (or priority 0)
        :add-on? add-on
        :multi-root t
        :initialization-options initialization-options)))

    (defun dotfiles/lsp-rooted-p (file-name markers)
      (and file-name
           (seq-some (lambda (marker)
                       (locate-dominating-file file-name marker))
                     markers)))

    (defun dotfiles/lsp-eslint-activate-p (file-name _mode)
      (and file-name
           (string-match-p "\\.\\(?:[cm]?[jt]sx?\\|vue\\)\\'" file-name)
           (dotfiles/lsp-rooted-p
            file-name
            '("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs"
              "eslint.config.ts" "eslint.config.mts" "eslint.config.cts"
              ".eslintrc" ".eslintrc.js" ".eslintrc.cjs" ".eslintrc.json"
              ".eslintrc.yaml" ".eslintrc.yml"))))

    (defun dotfiles/lsp-oxlint-activate-p (file-name _mode)
      (and file-name
           (string-match-p "\\.\\(?:astro\\|[cm]?[jt]sx?\\|svelte\\|vue\\)\\'" file-name)
           (dotfiles/lsp-rooted-p file-name '(".oxlintrc.json" ".oxlintrc.jsonc"))))

    (dolist (mapping
             '((clojure-mode . "clojure")
               (clojure-ts-mode . "clojure")
               (fennel-mode . "fennel")
               (go-mode . "go")
               (go-ts-mode . "go")
               (go-mod-ts-mode . "go.mod")
               (haskell-mode . "haskell")
               (haskell-ts-mode . "haskell")
               (literate-haskell-mode . "literate haskell")
               (kotlin-mode . "kotlin")
               (kotlin-ts-mode . "kotlin")
               (lua-mode . "lua")
               (lua-ts-mode . "lua")
               (markdown-mode . "markdown")
               (gfm-mode . "markdown")
               (nix-mode . "nix")
               (nix-ts-mode . "nix")
               (perl-mode . "perl")
               (cperl-mode . "perl")
               (terraform-mode . "terraform")
               (hcl-mode . "terraform")
               (LaTeX-mode . "latex")
               (latex-mode . "latex")
               (plain-tex-mode . "plaintex")
               (typst-ts-mode . "typst")
               (yaml-mode . "yaml")
               (yaml-ts-mode . "yaml")
               (zig-mode . "zig")
               (zig-ts-mode . "zig")))
      (add-to-list 'lsp-language-id-configuration mapping))

    (dotfiles/lsp-register 'dotfiles-bash '(sh-mode bash-ts-mode))
    (dotfiles/lsp-register 'dotfiles-clangd '(c-mode c-ts-mode c++-mode c++-ts-mode))
    (dotfiles/lsp-register 'dotfiles-clojure '(clojure-mode clojure-ts-mode))
    (dotfiles/lsp-register 'dotfiles-fennel '(fennel-mode))
    (dotfiles/lsp-register 'dotfiles-fish '(fish-mode))
    (dotfiles/lsp-register 'dotfiles-gopls '(go-mode go-ts-mode go-mod-ts-mode))
    (dotfiles/lsp-register 'dotfiles-haskell '(haskell-mode haskell-ts-mode literate-haskell-mode))
    (dotfiles/lsp-register 'dotfiles-json '(js-json-mode json-ts-mode))
    (dotfiles/lsp-register 'dotfiles-kotlin '(kotlin-mode kotlin-ts-mode))
    (dotfiles/lsp-register 'dotfiles-lua '(lua-mode lua-ts-mode))
    (dotfiles/lsp-register 'dotfiles-marksman '(markdown-mode gfm-mode))
    (dotfiles/lsp-register 'dotfiles-nixd '(nix-mode nix-ts-mode))
    (dotfiles/lsp-register 'dotfiles-perl-navigator '(perl-mode cperl-mode) :priority 1)
    (dotfiles/lsp-register 'dotfiles-perl-language-server '(perl-mode cperl-mode) :add-on t)
    (dotfiles/lsp-register 'dotfiles-rust-analyzer '(rust-mode rust-ts-mode))
    (dotfiles/lsp-register 'dotfiles-terraform '(terraform-mode hcl-mode))
    (dotfiles/lsp-register 'dotfiles-texlab '(LaTeX-mode latex-mode plain-tex-mode))
    (dotfiles/lsp-register 'dotfiles-tinymist '(typst-ts-mode))
    (dotfiles/lsp-register
     'dotfiles-typescript nil
     :activation-fn (lsp-activate-on "javascript" "javascriptreact" "typescript" "typescriptreact")
     :initialization-options
     '(:disableAutomaticTypingAcquisition t
       :tsserver (:path "${pkgs.typescript}/lib/node_modules/typescript/lib/tsserver.js")))
    (dotfiles/lsp-register 'dotfiles-vim '(vimrc-mode vimscript-ts-mode))
    (dotfiles/lsp-register 'dotfiles-yaml '(yaml-mode yaml-ts-mode))
    (dotfiles/lsp-register 'dotfiles-zls '(zig-mode zig-ts-mode))
    (dotfiles/lsp-register 'dotfiles-zuban '(python-mode python-ts-mode) :priority 1)
    (dotfiles/lsp-register 'dotfiles-ruff '(python-mode python-ts-mode) :add-on t)
    (dotfiles/lsp-register 'dotfiles-oxlint nil :add-on t :activation-fn #'dotfiles/lsp-oxlint-activate-p)

    ;; ESLint has protocol-specific request handlers.  Load only that explicit
    ;; client, replace its command with the Nix executable, and remove its
    ;; downloader before any buffer can start it.
    (setq lsp-eslint-server-command (dotfiles/lsp-command 'eslint))
    (require 'lsp-eslint)
    (let ((client (gethash 'eslint lsp-clients)))
      (unless client
        (error "lsp-eslint did not register its client"))
      (setf (lsp--client-download-server-fn client) nil
            (lsp--client-activation-fn client) #'dotfiles/lsp-eslint-activate-p))

    (defconst dotfiles-lsp-modes
      '(sh-mode bash-ts-mode c-mode c-ts-mode c++-mode c++-ts-mode
        clojure-mode clojure-ts-mode fennel-mode fish-mode go-mode go-ts-mode
        go-mod-ts-mode haskell-mode haskell-ts-mode literate-haskell-mode
        js-mode js-ts-mode js-jsx-mode typescript-mode
        typescript-ts-mode web-mode js-json-mode json-ts-mode kotlin-mode
        kotlin-ts-mode lua-mode lua-ts-mode markdown-mode gfm-mode nix-mode
        nix-ts-mode perl-mode cperl-mode python-mode python-ts-mode rust-mode
        rust-ts-mode terraform-mode hcl-mode LaTeX-mode latex-mode
        plain-tex-mode typst-ts-mode vimrc-mode vimscript-ts-mode yaml-mode
        yaml-ts-mode zig-mode zig-ts-mode))

    (defconst dotfiles-lsp-format-on-save-modes
      '(sh-mode bash-ts-mode c-mode c-ts-mode c++-mode c++-ts-mode
        clojure-mode clojure-ts-mode fish-mode go-mode go-ts-mode go-mod-ts-mode
        js-mode js-ts-mode js-jsx-mode typescript-mode
        typescript-ts-mode web-mode js-json-mode json-ts-mode lua-mode
        lua-ts-mode markdown-mode gfm-mode perl-mode cperl-mode python-mode
        python-ts-mode rust-mode rust-ts-mode terraform-mode hcl-mode
        LaTeX-mode latex-mode plain-tex-mode typst-ts-mode yaml-mode
        yaml-ts-mode zig-mode zig-ts-mode))

    (defun dotfiles/lsp-format-buffer-if-supported ()
      (when (and (bound-and-true-p lsp-mode)
                 (lsp-feature? "textDocument/formatting"))
        (lsp-format-buffer)))

    (defun dotfiles/lsp-mode-setup ()
      (unless (file-remote-p default-directory)
        (lsp-deferred)
        (when (memq major-mode dotfiles-lsp-format-on-save-modes)
          (add-hook 'before-save-hook #'dotfiles/lsp-format-buffer-if-supported nil t))))

    (dolist (mode dotfiles-lsp-modes)
      (add-hook (intern (format "%s-hook" mode)) #'dotfiles/lsp-mode-setup))

    (provide 'dotfiles-lsp-servers)
    ;;; lsp-servers.el ends here
  '';
}
