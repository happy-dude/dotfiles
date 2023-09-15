;; org-mode

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'load-path "~/.config/emacs/plugins/org-mode/lisp")
(add-to-list 'load-path "~/.config/emacs/plugins/org-mode/lisp" t)
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

(require 'org)
(require 'org-tempo)
(setq org-log-done t)

;; Start as org-mode for default mode
(setq-default major-mode 'org-mode)
