;;; secret-state-test.el --- Exercise sensitive file state -*- lexical-binding: t; -*-

;; emacs/check.nix names lib/rainbow-delimiters.nix as JSON in
;; DOTFILES_RAINBOW_PALETTE. Every depth must get its colour, and faces past
;; the palette would keep the package defaults, so the cycle must stop at
;; the palette's length.
(let ((palette (with-temp-buffer
                 (insert-file-contents (getenv "DOTFILES_RAINBOW_PALETTE"))
                 (json-parse-buffer :object-type 'alist))))
  (when (zerop (length palette))
    (error "The rainbow palette is empty"))
  (seq-do-indexed
   (lambda (colour index)
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" (1+ index))))
           (expected (alist-get 'gui colour)))
       (unless (equal (downcase (or (face-attribute face :foreground nil t) ""))
                      (downcase expected))
         (error "%s is not %s" face expected))))
   palette)
  (unless (equal rainbow-delimiters-max-face-count (length palette))
    (error "rainbow-delimiters-max-face-count does not match the palette")))
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#112233")))))
(unless (equal (face-attribute 'rainbow-delimiters-depth-1-face :foreground nil t)
               "#112233")
  (error "custom-set-faces did not override the declarative rainbow face"))

;; Loaded after init.el with the check's disposable HOME and XDG directories.
(defun dotfiles-test/exercise-file-state (file sensitive &optional renamed-file mode)
  "Edit FILE and check actual state copies.
Rename the buffer to RENAMED-FILE and then switch to major MODE first, when
given."
  ;; Batch visits otherwise skip the normal auto-save initialization.
  (let ((buffer (let ((noninteractive nil))
                  (find-file-noselect file))))
    (unwind-protect
        (with-current-buffer buffer
          (when renamed-file
            (set-visited-file-name renamed-file t))
          (when mode
            (funcall mode))
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
  (dotfiles-test/exercise-file-state
   secret nil (expand-file-name "~/declassified.txt"))
  (dotfiles-test/exercise-file-state secret t nil #'fundamental-mode)
  ;; Visiting secrets must not disable state for subsequent ordinary buffers.
  (dotfiles-test/exercise-file-state ordinary nil))

(dolist (file '("~/.config/opencode/local.json" "~/.omp/agent/agent.db"))
  (unless (dotfiles/sensitive-file-p (expand-file-name file))
    (error "Sensitive path is not guarded: %s" file)))

;; The guard lifts only its own override: an ordinary file whose directory
;; locals turn undo history off keeps that setting.
(let* ((project (expand-file-name "~/no-undo-history/"))
       (file (expand-file-name "notes.txt" project))
       (enable-local-variables :all))
  (make-directory project t)
  (with-temp-file (expand-file-name ".dir-locals.el" project)
    (insert "((nil . ((undo-tree-auto-save-history . nil))))\n"))
  (with-temp-file file
    (insert "initial fixture\n"))
  (with-current-buffer (find-file-noselect file)
    (unwind-protect
        (unless (and (local-variable-p 'undo-tree-auto-save-history)
                     (null undo-tree-auto-save-history))
          (error "Guard discarded a directory-local undo history setting"))
      (kill-buffer))))

;;; secret-state-test.el ends here
