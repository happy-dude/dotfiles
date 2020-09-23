;; rainbow-delimiters
(add-to-list 'load-path "~/.config/emacs/plugins/rainbow-delimiters")
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
