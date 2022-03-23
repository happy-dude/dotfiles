;; slime

(load (expand-file-name "~/.roswell/helper.el"))
(add-to-list 'load-path "~/.config/emacs/plugins/slime")
(require 'slime)
(require 'slime-autoloads)
;(setq inferior-lisp-program "sbcl")
(setq inferior-lisp-program "ros -Q run")
