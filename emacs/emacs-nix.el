;; auto-save settings
;; create cache directories if they do not exist
(let ((cache-dir "~/.local/share/emacs")
      (backup-dir "~/.local/share/emacs/backup")
      (auto-save-dir "~/.local/share/emacs/autosave"))
  (dolist (dir (list cache-dir backup-dir auto-save-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  )

(setq backup-directory-alist `((".*" . ,"~/.local/share/emacs/backup"))
      make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      )
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.local/share/emacs/autosave/") t))
      auto-save-directory-fallback "/tmp/"
      auto-save-hash-p nil
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-interval 300            ; number of keystrokes between auto-saves (default: 300)
      auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
      )

;; Values added by Custom
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )

;; Mimic vim rainbow parentheses settings:
;; red, green, blue-green, red-orange, blue, orange, violet, yellow, red-violet
;; Matching paren as azure
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(rainbow-delimiters-depth-1-face ((t (:foreground "#FE2712"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "#66B032"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "#0392CE"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "#FD5308"))))
  '(rainbow-delimiters-depth-5-face ((t (:foreground "#0247FE"))))
  '(rainbow-delimiters-depth-6-face ((t (:foreground "#FB9902"))))
  '(rainbow-delimiters-depth-7-face ((t (:foreground "#8601AF"))))
  '(rainbow-delimiters-depth-8-face ((t (:foreground "#FEFE33"))))
  '(rainbow-delimiters-depth-9-face ((t (:foreground "#A7194B"))))
  '(rainbow-delimiters-unmatched-face ((t (:background "#D0EA2B"))))
  '(show-paren-match ((t (:foreground "azure" :weight semi-bold))))
  )

;; Settings related to visuals, line numbers, fonts, etc.
;; Hide welcome screen
(setq inhibit-startup-screen t)

;; Highlight current line
(global-hl-line-mode +1)

;; Always show matching parentheses
(show-paren-mode t)
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

;; Solarized theme
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
;(load-theme 'solarized-dark t)
(load-theme 'solarized-gruvbox-dark t)

; editorconfig settings
(editorconfig-mode 1)

;; macOS globalcapture support
;; See https://www.reddit.com/r/emacs/comments/6lzyg2/heres_how_to_do_emacsclient_global_orgcapture/ and https://cestlaz.github.io/posts/using-emacs-24-capture-2/
;; Bind app to:
;; /usr/local/bin/emacs --daemon 2>&1 &
;; Bind quick action to:
;; /usr/local/bin/emacsclient -s $(lsof -c Emacs | grep server | tr -s " " | cut -d' ' -f 8) -c -ne "(make-capture-frame)" >/dev/null 2>&1 &

;; Quick-capture for macOS
;;      Use Automator to create an Application that runs a shell script starting emacs as a daemon
;;      Bind that Application as a login item on startup for the user
;;      Use Automator to create a Quick Action that runs a shell script that starts emacsclient binded to daemon with org-capture
;;      Bind that Quick Action to a keyboard shortcut in Keyboard settings under System Preferences

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
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))

;; org-mode
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq
  org-src-fontify-natively t
  org-src-preserve-indentation t
  org-src-tab-acts-natively t
  ;org-startup-folded nil
  org-startup-indented t
  org-directory "~/org"
  org-default-notes-file (concat org-directory "/notes.org")
  )
(setq org-agenda-files (append (list "~/org/notes.org"
                                     "~/org/work.org"
                                     "~/org/cal.org")))
;(directory-files-recursively "~/org/roam/" "\\.org$")))
(setq org-capture-templates
      '(
        ("t" "Notes Unfiled" entry (file+headline "~/org/notes.org" "Unfiled")
         "* TODO %?\n%U" :prepend t)
        ("u" "Work Unfiled" entry (file+headline "~/org/work.org" "Unfiled")
         "* TODO %?\n%U" :prepend t)

        ("g" "GTD")
        ("ga" "Actioning" entry (file+headline "~/org/notes.org" "Actioning")
         "* TODO %?")
        ("gp" "Projects" entry (file+headline "~/org/notes.org" "Projects")
         "* IDEA %?\n%U\n")
        ("gf" "Focuses" entry (file+headline "~/org/notes.org" "Focuses")
         "* %?" :prepend t)
        ("gg" "Goals" entry (file+headline "~/org/notes.org" "Goals")
         "* TODO %?" :prepend t)
        ("gv" "Visions" entry (file+headline "~/org/notes.org" "Visions")
         "* IDEA %?" :prepend t)
        ("gl" "Life" entry (file+headline "~/org/notes.org" "Life")
         "* %?" :prepend t)

        ("r" "Reading list" entry (file+headline "~/org/notes.org" "Reading List")
         "* TODO %?\n" :prepend t)
        ("w" "Watching list" entry (file+headline "~/org/notes.org" "Watching List")
         "* TODO %?\n" :prepend t)

        ("f" "Financial")
        ("fs" "Subscriptions" entry (file+headline "~/org/notes.org" "Subscriptions")
         "* %?")
        ("fg" "Goals" entry (file+olp "~/org/notes.org" "Financial" "Goals")
         "* TODO %?" :prepend t)

        ("s" "Shopping")
        ("sg" "Groceries" entry (file+headline "~/org/notes.org" "Groceries")
         "* WISHLIST %?" :prepend t)
        ("so" "Ordering" entry (file+headline "~/org/notes.org" "Ordering")
         "* WISHLIST %?" :prepend t)
        ("sw" "Wishlist" entry (file+headline "~/org/notes.org" "Wishlist")
         "* WISHLIST %?" :prepend t)
        ("sg" "Gifts" entry (file+headline "~/org/notes.org" "Gifts")
         "* WISHLIST %?" :prepend t)

        ("e" "Cloudflare")
        ("et" "Tickets" entry (file+headline "~/org/work.org" "Tickets")
         "* [[https://jira.cfops.it/browse/%?" :prepend t)
        ("ei" "Incidents" entry (file+headline "~/org/work.org" "Incidents")
         "* DETECT [[https://jira.cfops.it/browse/INCIDENT-%?" :prepend t)
        ("ew" "Watching" entry (file+headline "~/org/work.org" "Watching")
         "* %?\n%U" :prepend t)
        ("ed" "Drive-by" entry (file+headline "~/org/work.org" "Drive-by")
         "* TODO %?\n%U" :prepend t)
        ("el" "Learning" entry (file+headline "~/org/work.org" "Learn")
         "* TODO %?" :prepend t)
        ("e1" "1:1" entry (file+headline "~/org/work.org" "1:1")
         "* %U\n%?- " :prepend t)
        ("eo" "OKRs" entry (file+headline "~/org/work.org" "OKRs")
         "* TODO %?")
        ("ep" "Projects" entry (file+headline "~/org/work.org" "Projects")
         "* IDEA %?" :prepend t)
        ("ee" "Expenses" entry (file+headline "~/org/work.org" "Expenses")
         "* TODO %?" :prepend t)

        ("c" "Career Development")
        ("ce" "Resume" entry (file+headline "~/org/work.org" "Resume")
         "* TODO %?")
        ("cv" "Visions" entry (file+headline "~/org/work.org" "Visions")
         "* TODO %?\n%U" :prepend t)
        ("cl" "Life" entry (file+headline "~/org/work.org" "Life")
         "* %?\n%U" :prepend t)
        ))
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
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

; Make org-mode, iOS BeOrg, emacs, and (Drop)Box sync play well with each other
; See https://christiantietze.de/posts/2019/03/sync-emacs-org-files/ and https://www.nicklanasa.com/posts/emacs-syncing-dropbox-beorg
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

(setq org-log-done t)

;; Start as org-mode for default mode
(setq-default major-mode 'org-mode)

;; org-roam settings
;; config
;; acknowledge v2 migration
(setq org-roam-v2-ack t)

;; update last_modified when saving
;; ref: https://github.com/emacs-mirror/emacs/blob/master/lisp/time-stamp.el#L44-L73
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'time-stamp-pattern)
                 "8/^#\\+LAST_MODIFIED: %%$")
            (set (make-local-variable 'time-stamp-format)
                 "[%Y-%02m-%02d %3a %02I:%02M:%02S %P %Z] - %L")
            (add-hook 'before-save-hook 'time-stamp nil 'local)))

;; ref: https://www.orgroam.com/manual/Getting-Started.html#Getting-Started
(setq org-roam-directory "~/org/roam")
(setq org-roam-db-location "~/org/roam/org-roam.db")
(setq org-roam-graph-executable "/usr/local/bin/dot")
(setq org-roam-graph-viewer '(lambda (file)
                               (let ((file-file (concat "file://" file)))
                                 (call-process
                                   "/Applications/Firefox Nightly.app/Contents/MacOS/firefox"
                                   nil
                                   0
                                   nil
                                   file-file))))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d>-$\{slug}.org"
                            ":PROPERTIES:
                            :ID: %(org-id-new)
                            :ROAM_REFS: $\{ref}
                            :END:
                            #+TITLE: $\{title}
                            #+CREATED: %U
                            #+LAST_MODIFIED: %U
                            #+FILETAGS:
                            - sources ::
                            -
                            - nodes ::

                            * Summary
                            ** Questions
                            ** Impact
                            *** Purpose (Inspire)
                            *** Values (Guide)
                            *** Habits (Define)

                            * Notes
                            -")
                            :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain "%?"
         :if-new (file+head "%<%Y%m%d>-$\{slug}.org"
                            ":PROPERTIES:
                            :ID: %(org-id-new)
                            :ROAM_REFS: $\{ref}
                            :END:
                            #+TITLE: $\{title}
                            #+CREATED: %U
                            #+LAST_MODIFIED: %U
                            #+FILETAGS:
                            - sources ::
                            -
                            - nodes ::

                            * Summary
                            ** Questions
                            ** Impact
                            *** Purpose (Inspire)
                            *** Values (Guide)
                            *** Habits (Define)

                            * Notes
                            -")
                            :unnarrowed t)))


(define-key global-map (kbd "C-c n l") 'org-roam-buffer-toggle)
(define-key global-map (kbd "C-c n f") 'org-roam-node-find)
(define-key global-map (kbd "C-c n g") 'org-roam-graph)
(define-key global-map (kbd "C-c n i") 'org-roam-node-insert)
(define-key global-map (kbd "C-c n c") 'org-roam-capture)

(add-hook 'after-init-hook 'org-roam-db-autosync-mode)

;; org-roam-ui settings
;(setq org-roam-ui-sync-theme t
;      org-roam-ui-follow t
;      org-roam-ui-update-on-save t
;      org-roam-ui-open-on-start t)
;; org-babel settings

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

;; ref: https://orgmode.org/manual/Languages.html
;; https://orgmode.org/worg/org-contrib/babel/languages.html
(org-babel-do-load-languages
  'org-babel-load-languages
  '(
    ;; Core
    ;(abc         . t)
    ;(asymptote   . t)
    (awk         . t)
    (C           . t)  ; enables C, C++, and D
    (calc        . t)
    ;(clojure     . t)
    ;(comint      . t)
    ;(coq         . t)
    (css         . t)
    ;(ditaa       . t)
    (dot         . t)
    ;(ebnf        . t)
    (emacs-lisp  . t)
    (eshell      . t)
    ;(forth       . t)
    ;(fortran     . t)
    ;(F90         . t)
    ;(gnuplot     . t)
    (haskell     . t)
    ;(io          . t)
    ;(J           . t)
    ;(java        . t)
    (js          . t)
    (latex       . t)
    (lua         . t)
    ;(ledger      . t)
    ;(ly          . t)
    (lisp        . t)
    (makefile    . t)
    ;(matlab      . t)
    ;(max         . t)
    ;(mscgen      . t)
    ;(ocaml       . t)
    ;(octave      . t)
    (org         . t)
    (perl        . t)
    ;(picolisp    . t)
    ;(plantuml    . t)
    ;(processing  . t)
    (python      . t)
    ;(R           . t)
    (ruby        . t)
    ;(sass        . t)
    ;(scala       . t)
    ;(scheme      . t)
    ;(screen      . t)
    (sed         . t)
    (shell       . t)
    ;(shen        . t)
    (sql         . t)
    (sqlite      . t)
    ;(stan        . t)
    ;(vala        . t)

    ;; Contributed
    ;(eukleides   . t)
    ;(fomus       . t)
    ;(groovy      . t)
    ;(julia       . t)
    ;(mathematica . t)
    ;(mathomatic  . t)
    ;(oz          . t)
    ;(stata       . t)
    ;(tcl         . t)

    ;; Emacs Package
    ;(axiom       . t)
    ;(browser     . t)
    ;(cypher      . t)
    ;(elixir      . t)
    ;(http        . t)
    ;(ipython     . t)
    ;(kotlin      . t)
    ;(lfe         . t)
    ;(mongo       . t)
    ;(prolog      . t)
    ;(rec         . t)
    ;(sml         . t)
    ;(translate   . t)
    ;(typescript  . t)
    ))

;; org-journal
;; config
;; ref: https://github.com/bastibe/org-journal
(setq org-journal-file-type 'weekly)
(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (org-id-get-create)
    (pcase org-journal-file-type
           (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
           (`weekly (concat "#+TITLE: Weekly Journal - " (format-time-string "%Y-W%V") "\n#+STARTUP: folded"))
           (`monthly (concat "#+TITLE: Monthly Journal - " (format-time-string "%B [%Y%m]") "\n#+STARTUP: folded"))
           (`yearly (concat "#+TITLE: Yearly Journal - " (format-time-string "%Y") "\n#+STARTUP: folded"))
           )))
(setq org-journal-file-header 'org-journal-file-header-func)

(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-time-format "%r %Z ")
(setq org-journal-dir "~/org/roam/journal/")
(setq org-journal-file-format "%Y-W%V.org")
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

;; evil-mode settings
;; Make sure to set `evil-want-integration' to nil before loading evil or evil-collection.
;; Warning (evil-collection): Make sure to set `evil-want-keybinding' to nil before loading evil or evil-collection.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; evil-mode
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(evil-mode 1)

;; evil-collection
(evil-collection-init)

;; evil-surround
(global-evil-surround-mode 1)

(add-hook 'org-mode-hook 'evil-org-mode)
(add-hook 'evil-org-mode-hook
          (lambda ()
            (evil-org-set-key-theme '(navigation
                                       insert
                                       textobjects
                                       additional
                                       calendar
                                       ))
            (evil-org-agenda-set-keys)))

;; evil-rsi
(evil-rsi-mode)

;; use undo-tree for evil-mode
(evil-set-undo-system 'undo-tree)
;; undo-tree

(global-undo-tree-mode)

;; slime
(load (expand-file-name "~/.roswell/helper.el"))
;(setq inferior-lisp-program "sbcl")
(setq inferior-lisp-program "ros -Q run")

;; which-key settings
(which-key-mode t)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

