;;; secret-state-test.el --- Exercise sensitive file state -*- lexical-binding: t; -*-

;; Loaded after init.el with the check's disposable HOME and XDG directories.
(defun dotfiles-test/exercise-file-state (file sensitive &optional renamed-file)
  "Edit FILE and check actual state copies, optionally after RENAMED-FILE."
  ;; Batch visits otherwise skip the normal auto-save initialization.
  (let ((buffer (let ((noninteractive nil))
                  (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (when renamed-file
            (set-visited-file-name renamed-file t))
          (let ((auto-save-file (make-auto-save-file-name))
                (history-file (undo-tree-make-history-save-file-name buffer-file-name))
                (backup-file (make-backup-file-name buffer-file-name)))
            (goto-char (point-max))
            (insert "updated fixture\n")
            (undo-boundary)
            (do-auto-save t t)
            (unless (eq (file-exists-p auto-save-file) (not sensitive))
              (error "Wrong auto-save behavior for fixture: %s" buffer-file-name))
            (save-buffer)
            (unless (eq (file-exists-p history-file) (not sensitive))
              (error "Wrong undo history behavior for fixture: %s" buffer-file-name))
            (when (and sensitive (file-expand-wildcards (concat backup-file "*")))
              (error "Sensitive fixture gained a backup: %s" buffer-file-name))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(let* ((secret-dir (expand-file-name "~/.config/rclone"))
       (secret (expand-file-name "credential.txt" secret-dir))
       (alias (expand-file-name "~/secret-alias.txt"))
       (directory-alias (expand-file-name "~/rclone-alias"))
       (ordinary (expand-file-name "~/ordinary.txt"))
       (renamed (expand-file-name "~/renamed.txt")))
  (make-directory secret-dir t)
  (make-directory (expand-file-name "emacs/undo-tree" (getenv "XDG_CACHE_HOME")) t)
  (dolist (file (list secret ordinary renamed))
    (with-temp-file file
      (insert "initial fixture\n")))
  (make-symbolic-link secret alias)
  (make-symbolic-link secret-dir directory-alias)
  (dotfiles-test/exercise-file-state ordinary nil)
  (dotfiles-test/exercise-file-state secret t)
  (dotfiles-test/exercise-file-state alias t)
  (dotfiles-test/exercise-file-state
   (expand-file-name "credential.txt" directory-alias) t)
  (dotfiles-test/exercise-file-state renamed t secret)
  ;; Visiting secrets must not disable state for subsequent ordinary buffers.
  (dotfiles-test/exercise-file-state ordinary nil))

(dolist (file '("~/.config/opencode/local.json" "~/.omp/agent/agent.db"))
  (unless (dotfiles/sensitive-file-p (expand-file-name file))
    (error "Sensitive path is not guarded: %s" file)))

;;; secret-state-test.el ends here
