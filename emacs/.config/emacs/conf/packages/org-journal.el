;; org-journal

(add-to-list 'load-path "~/.config/emacs/plugins/org-journal")

;; config
;; ref: https://github.com/bastibe/org-journal
(setq org-journal-file-type 'weekly)
(defun org-journal-file-header-func (time)
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly (concat "#+TITLE: Weekly Journal - " (format-time-string "%Y-W%V") "\n#+STARTUP: folded"))
      (`monthly (concat "#+TITLE: Monthly Journal - " (format-time-string "%B [%Y%m]") "\n#+STARTUP: folded"))
      (`yearly (concat "#+TITLE: Yearly Journal - " (format-time-string "%Y") "\n#+STARTUP: folded"))
      )))
(setq org-journal-file-header 'org-journal-file-header-func)

(setq org-journal-date-format "%A, %d %B %Y")
(setq org-journal-time-format "%r %Z ")
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y-W%V.org")

(require 'org-journal)
