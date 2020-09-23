;; Solarized theme
(add-to-list 'load-path "~/.config/emacs/plugins/dash.el")
(require 'dash)
(require 'dash-functional)
(add-to-list 'load-path "~/.config/emacs/themes/solarized-emacs")
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/solarized-emacs")
(require 'solarized)
(add-hook 'org-mode-hook
          (lambda () (set-frame-font "Fira Code Retina 16")))
(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      ;; solarized-height-minus-1 1.0
      ;; solarized-height-plus-1 1.0
      ;; solarized-height-plus-2 1.0
      ;; solarized-height-plus-3 1.0
      ;; solarized-height-plus-4 1.0)
      )
(load-theme 'solarized-dark t)
