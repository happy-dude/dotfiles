;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq conf-dir "~/.config/emacs/conf/")
(setq package-conf-dir "~/.config/emacs/conf/packages/")

;; Load emacs configs
(load-file (concat conf-dir "auto-save.el"))
(load-file (concat conf-dir "custom.el"))
(load-file (concat conf-dir "visual.el"))

;; Load emacs packages configs
(load-file (concat package-conf-dir "annalist.el"))
(load-file (concat package-conf-dir "editorconfig.el"))
(load-file (concat package-conf-dir "evil-mode.el"))
(load-file (concat package-conf-dir "org-journal.el"))
(load-file (concat package-conf-dir "org-mode-capture.el"))
(load-file (concat package-conf-dir "org-mode.el"))
(load-file (concat package-conf-dir "org-roam.el"))
(load-file (concat package-conf-dir "rainbow.el"))
(load-file (concat package-conf-dir "slime.el"))
(load-file (concat package-conf-dir "solarized.el"))
(load-file (concat package-conf-dir "undo-tree.el"))
(load-file (concat package-conf-dir "which-key.el"))
