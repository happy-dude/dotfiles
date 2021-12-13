;; evil-mode settings

;; Make sure to set `evil-want-integration' to nil before loading evil or evil-collection.
;; Warning (evil-collection): Make sure to set `evil-want-keybinding' to nil before loading evil or evil-collection.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; evil-mode
(add-to-list 'load-path "~/.config/emacs/plugins/evil")
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(require 'evil)
(evil-mode 1)

;; evil-collection
(add-to-list 'load-path "~/.config/emacs/plugins/evil-collection")
(require 'evil-collection)
(evil-collection-init)

;; evil-surround
(add-to-list 'load-path "~/.config/emacs/plugins/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

(add-to-list 'load-path "~/.config/emacs/plugins/evil-org-mode")
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; evil-rsi
(add-to-list 'load-path "~/.config/emacs/plugins/evil-rsi")
(require 'evil-rsi)
(evil-rsi-mode)

;; use undo-tree for evil-mode
(evil-set-undo-system 'undo-tree)
