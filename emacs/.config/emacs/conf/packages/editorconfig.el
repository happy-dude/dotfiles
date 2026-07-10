; editorconfig settings

(add-to-list 'load-path "~/.config/emacs/plugins/editorconfig-emacs/")
(require 'editorconfig)
;; Preserve Emacs' form-aware indentation at the conventional Lisp width.
(setq editorconfig-lisp-use-default-indent 2)
(editorconfig-mode 1)
