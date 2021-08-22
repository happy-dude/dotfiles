;; elisp-tree-sitter

(add-to-list 'load-path "~/.config/emacs/plugins/elisp-tree-sitter/core")
(add-to-list 'load-path "~/.config/emacs/plugins/elisp-tree-sitter/lisp")
(add-to-list 'load-path "~/.config/emacs/plugins/elisp-tree-sitter/langs")

(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
