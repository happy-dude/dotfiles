;; magit settings

;; dependencies
(add-to-list 'load-path "~/.config/emacs/plugins/dash.el")
(require 'dash)

(add-to-list 'load-path "~/.config/emacs/plugins/libegit2")
(require 'libgit)

(add-to-list 'load-path "~/.config/emacs/plugins/transient/lisp")
(require 'transient)

(add-to-list 'load-path "~/.config/emacs/plugins/with-editor")
(require 'with-editor)

(add-to-list 'load-path "~/.config/emacs/plugins/magit/lisp")
(require 'magit)

;; magit documentation
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.config/emacs/plugins/magit/Documentation/"))
