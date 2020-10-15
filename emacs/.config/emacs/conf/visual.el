;; Settings related to visuals, line numbers, fonts, etc.

;; Hide welcome screen
(setq inhibit-startup-screen t)

;; Highlight current line
(global-hl-line-mode +1)

;; Always show matching parentheses
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Line numbers
(setq-default display-line-numbers 'relative)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Disable line-wrapping
(set-default 'truncate-lines t)

;; Enable mouse support in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  ;; enable mouse-based scrolling
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Set default font
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Retina")))
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Retina")
(add-to-list 'default-frame-alist
             '(font . "Fira Code Retina-16"))
