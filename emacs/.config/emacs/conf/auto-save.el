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
