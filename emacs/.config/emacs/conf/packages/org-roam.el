;; org-roam settings

;; dependencies
(add-to-list 'load-path "~/.config/emacs/plugins/dash.el")
(require 'dash)

(add-to-list 'load-path "~/.config/emacs/plugins/s.el")
(require 's)

(add-to-list 'load-path "~/.config/emacs/plugins/f.el")
(require 'f)

(add-to-list 'load-path "~/.config/emacs/plugins/emacsql")
(require 'emacsql)

(add-to-list 'load-path "~/.config/emacs/plugins/emacsql-sqlite3")
(require 'emacsql-sqlite3)

(add-to-list 'load-path "~/.config/emacs/plugins/compat")
(require 'compat)

(add-to-list 'load-path "~/.config/emacs/plugins/magit/lisp")
(require 'magit-section)

(add-to-list 'load-path "~/.config/emacs/plugins/org-roam")
(add-to-list 'load-path "~/.config/emacs/plugins/org-roam/extensions")

;; config

;; acknowledge v2 migration
(setq org-roam-v2-ack t)

;; update last_modified when saving
;; ref: https://github.com/emacs-mirror/emacs/blob/master/lisp/time-stamp.el#L44-L73
(require 'time-stamp)
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
         :if-new (file+head "%<%Y%m%d>-${slug}.org"
":PROPERTIES:
:ID: %(org-id-new)
:ROAM_REFS: ${ref}
:END:
#+TITLE: ${title}
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
         :if-new (file+head "%<%Y%m%d>-${slug}.org"
":PROPERTIES:
:ID: %(org-id-new)
:ROAM_REFS: ${ref}
:END:
#+TITLE: ${title}
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

(require 'org-roam)
(require 'org-roam-graph)
(require 'org-roam-protocol)

(define-key global-map (kbd "C-c n l") 'org-roam-buffer-toggle)
(define-key global-map (kbd "C-c n f") 'org-roam-node-find)
(define-key global-map (kbd "C-c n g") 'org-roam-graph)
(define-key global-map (kbd "C-c n i") 'org-roam-node-insert)
(define-key global-map (kbd "C-c n c") 'org-roam-capture)

(add-hook 'after-init-hook 'org-roam-db-autosync-mode)

;; org-roam-ui settings
;(add-to-list 'load-path "~/.config/emacs/plugins/emacs-web-server")
;(require 'simple-httpd)
;
;(add-to-list 'load-path "~/.config/emacs/plugins/emacs-websocket")
;(require 'websocket)
;
;(add-to-list 'load-path "~/.config/emacs/plugins/org-roam-ui")
;(require 'org-roam-ui)
;
;(setq org-roam-ui-sync-theme t
;      org-roam-ui-follow t
;      org-roam-ui-update-on-save t
;      org-roam-ui-open-on-start t)
