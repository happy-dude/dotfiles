;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Hide welcome screen
(setq inhibit-startup-screen t)

;; Highlight current line
(global-hl-line-mode +1)

;; Backup directories
;; See https://stackoverflow.com/a/18330742 and https://snarfed.org/gnu_emacs_backup_files
(let ((cache-dir "~/.cache/emacs")
      (backup-dir "~/.cache/emacs/backup")
      (auto-save-dir "~/.cache/emacs/autosave"))
  (dolist (dir (list cache-dir backup-dir auto-save-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  )

(setq backup-directory-alist `((".*" . ,"~/.cache/emacs/backup"))
      make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      )
(setq auto-save-directory `((".*" . ,"~/.cache/emacs/autosave"))
      auto-save-file-name-transforms `((".*" ,"~/.cache/emacs/autosave" t))
      ;;auto-save-hash-directory (concat cache-dir "/autosave-hash")
      auto-save-directory-fallback "/tmp/"
      ;;auto-save-list-file-prefix (concat cache-dir "/autosave-")
      auto-save-hash-p nil
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      )

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.emacs.d/plugins/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/org-mode/lisp" t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq 
  org-src-fontify-natively t
  org-src-preserve-indentation t
  org-src-tab-acts-natively t
  org-startup-folded nil
  org-startup-indented t
  org-directory "~/org"
  org-default-notes-file (concat org-directory "/notes.org")
  )
(setq org-agenda-files (list "~/org/notes.org"
                             "~/org/personal.org"
                             "~/org/work.org")
      )
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/notes.org" "Unfiled")
         "* TODO %?\n  %i\n  %a"))
      )
(setq org-todo-keywords 
      '(
        (sequence "TODO" "PROCESS" "ORGANIZE" "REVIEW" "DO" "|" "DONE")                 ; GTD
        (sequence "|" "NOTDOING" "DELEGATED" "WAITING" "SOMEDAY")                       ; Blockers
        (sequence "IDEA" "DESIGN" "DEVELOP" "QA" "BACKLOG" "|" "COMPLETE")              ; Agile
        (sequence "DETECT" "TRIAGE" "CONTAIN" "ERADICATE" "|" "RECOVER" "POSTMORTEM")   ; Incident Response
        (sequence "RECON" "PAYLOAD" "DELIVER" "EXPLOIT" "INSTALL" "|" "C2" "ACTION")    ; Cyber Kill-Chain
        (sequence "WISHLIST" "CART" "SHIPPING" "ATTN" "|" "DONE" "RETURNED")            ; Shopping
        ))
(setq org-todo-keyword-faces '(
                               ("TODO" . "brown") ("PROCESS" . "royal blue") ("ORGANIZE" .  "chocolate1") ("REVIEW" ."MediumOrchid1") ("DO" . "brown1") ("DONE" . "DarkOliveGreen4")
                               ("NOTDOING" . "gray") ("DELEGATED" . "slate gray") ("WAITING" . "dim gray") ("SOMEDAY" . "dark slate gray")
                               ("IDEA" . "blue") ("DESIGN" . "orange") ("DEVELOP" . "red") ("QA" . "purple") ("BACKLOG" . "magenta") ("COMPLETE" . "green")
                               ("DETECT" . "steel blue") ("TRIAGE" . "dark orange") ("CONTAIN" . "IndianRed3") ("ERADICATE" . "orange red") ("RECOVER" . "violet") ("POSTMORTEM" . "chartreuse")
                               ("RECON" . "dark violet") ("PAYLOAD" . "pale violet red") ("DELIVER" . "firebrick") ("EXPLOIT" . "red") ("INSTALL" . "goldenrod") ("C2" . "lime green") ("ACTION" . "forest green")
                               ("WISHLIST" . "systemOrangeColor") ("CART" . "systemPurpleColor") ("SHIPPING" . "systemBlueColor") ("ATTN" . "systemRedColor") ("RETURNED" . "systemYellowColor")
                               ))
(require 'org)
(require 'org-tempo)
(setq org-log-done t)

; macOS globalcapture support
; See https://www.reddit.com/r/emacs/comments/6lzyg2/heres_how_to_do_emacsclient_global_orgcapture/ and https://cestlaz.github.io/posts/using-emacs-24-capture-2/
; Bind Key to: $(which emacsclient) -s $socketfile -c -a '' --eval "(make-capture-frame)"emacsclient -ne "(make-capture-frame)"
(defadvice org-capture-finalize
           (after delete-capture-frame activate)
           "Advise capture-finalize to close the frame"
           (if (equal "capture" (frame-parameter nil 'name))
               (delete-frame)))

(defadvice org-capture-destroy
           (after delete-capture-frame activate)
           "Advise capture-destroy to close the frame"
           (if (equal "capture" (frame-parameter nil 'name))
               (delete-frame)))

;; noflet
(add-to-list 'load-path "~/.emacs.d/plugins/emacs-noflet")
(require 'noflet)
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

;; Make sure to set `evil-want-integration' to nil before loading evil or evil-collection.
(setq evil-want-integration nil)

;; evil-mode
(add-to-list 'load-path "~/.emacs.d/plugins/evil")
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; evil-collection
(add-to-list 'load-path "~/.emacs.d/plugins/evil-collection")
(require 'evil-collection)
(evil-collection-init)

;; evil-surround
(add-to-list 'load-path "~/.emacs.d/plugins/evil-surround")
(require 'evil-surround)
(global-evil-surround-mode 1)

;; evil-org-mode
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; slime
(add-to-list 'load-path "~/.emacs.d/plugins/slime")
(require 'slime)
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")

;; Start as org-mode for default mode
(setq-default major-mode 'org-mode)
