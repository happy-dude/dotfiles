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

(add-to-list 'load-path "~/.config/emacs/plugins/org-roam")

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
      '(("d" "default" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d>-${slug}"
         :head
"#+TITLE: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+CREATED: %U
#+LAST_MODIFIED: %U
- source ::
  - ${ref}
- tags ::

* Summary
** Questions
** Impact
*** Purpose (Inspire)
*** Values (Guide)
*** Habits (Define)

* Notes
-"
         :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "%<%Y%m%d>-${slug}"
         :head
"#+TITLE: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_ALIAS:
#+ROAM_TAGS:
#+CREATED: %U
#+LAST_MODIFIED: %U
- source ::
  - ${ref}
- tags ::

* Summary
** Questions
** Impact
*** Purpose (Inspire)
*** Values (Guide)
*** Habits (Define)

* Notes
-"
         :unnarrowed t)))

(require 'org-roam)
(require 'org-roam-protocol)

(add-hook 'after-init-hook 'org-roam-mode)

(define-key org-roam-mode-map (kbd "C-c n l") 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") 'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n g") 'org-roam-graph)
(define-key org-mode-map (kbd "C-c n i") 'org-roam-insert)

(org-roam-setup)

;; org-roam-server settings
;(add-to-list 'load-path "~/.config/emacs/plugins/emacs-web-server")
;(require 'simple-httpd)
;(setq
;  httpd-root "/var/www"
;  httpd-port 8049)
;(httpd-start)
;
;(add-to-list 'load-path "~/.config/emacs/plugins/org-roam-server")
;(require 'org-roam-server)
;
;(setq org-roam-server-host "127.0.0.1"
;      org-roam-server-port 8049
;      org-roam-server-authenticate nil
;      org-roam-server-export-inline-images t
;      org-roam-server-serve-files nil
;      org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;      org-roam-server-network-poll t
;      org-roam-server-network-arrows nil
;      org-roam-server-network-label-truncate t
;      org-roam-server-network-label-truncate-length 60
;      org-roam-server-network-label-wrap-length 20)
