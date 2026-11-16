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
(require 'cl-lib)

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

;; emacs/check.nix names lib/sensitive-paths.nix as JSON in
;; DOTFILES_SENSITIVE_PATHS. The paths to probe come from that file rather
;; than from the generated sensitive-paths.el that init.el loads, so a wrong
;; generator cannot agree with itself.
(defconst dotfiles-test/sensitive-paths
  (with-temp-buffer
    (insert-file-contents (getenv "DOTFILES_SENSITIVE_PATHS"))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

;; XDG config entries are guarded under ~/.config and under the relocated
;; XDG_CONFIG_HOME the check exports.
(defconst dotfiles-test/config-home (getenv "XDG_CONFIG_HOME"))
(when (or (not (string-prefix-p "/" dotfiles-test/config-home))
          (equal dotfiles-test/config-home (expand-file-name "~/.config")))
  (error "The check did not relocate XDG_CONFIG_HOME"))

(defconst dotfiles-test/roots
  `((home ,(expand-file-name "~"))
    (xdgConfig ,(expand-file-name "~/.config") ,dotfiles-test/config-home))
  "The directories each kind of entry is looked up under.")

(defun dotfiles-test/entries (kind type)
  "Return the TYPE entries of KIND, failing when there are none."
  (or (alist-get type (alist-get kind dotfiles-test/sensitive-paths))
      (error "The sensitive path list has no %s %s" kind type)))

(defun dotfiles-test/check-sensitive (file)
  "Check that the guard recognizes FILE and refuses it a backup."
  (unless (dotfiles/sensitive-file-p file)
    (error "Sensitive path is not guarded: %s" file))
  (when (funcall backup-enable-predicate file)
    (error "Sensitive path may be backed up: %s" file)))

(defun dotfiles-test/check-sensitive-state (file)
  "Check that FILE is guarded and that editing it leaves no state copy."
  (dotfiles-test/check-sensitive file)
  ;; Writing or visiting a .gpg name would start GnuPG.
  (unless (string-match-p epa-file-name-regexp file)
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "initial fixture\n"))
    (dotfiles-test/exercise-file-state file t)))

(defun dotfiles-test/check-ordinary (file)
  "Check that the guard leaves FILE alone."
  (when (dotfiles/sensitive-file-p file)
    (error "Ordinary path is guarded: %s" file)))

(make-directory (expand-file-name "emacs/undo-tree" (getenv "XDG_CACHE_HOME")) t)
(pcase-dolist (`(,kind . ,roots) dotfiles-test/roots)
  (dolist (root roots)
    (dolist (entry (dotfiles-test/entries kind 'files))
      (let ((file (expand-file-name entry root)))
        (dotfiles-test/check-sensitive-state file)
        ;; A file entry is one file, not a name prefix.
        (dotfiles-test/check-ordinary (concat file ".sample"))))
    (dolist (entry (dotfiles-test/entries kind 'directories))
      (let ((directory (expand-file-name entry root)))
        (dotfiles-test/check-sensitive-state
         (expand-file-name "credential.txt" directory))
        (dotfiles-test/check-sensitive-state
         (expand-file-name "nested/credential.txt" directory))
        ;; A directory entry ends at a path separator.
        (dotfiles-test/check-ordinary
         (expand-file-name "ordinary.txt" (concat directory "-notes")))))))

;; A credential directory is guarded at any depth: in the home directory, in
;; a tree outside it such as another home or a mounted backup, and inside a
;; project.  A name that merely starts the same way is not.
(let ((elsewhere (expand-file-name "../elsewhere" "~")))
  (dolist (entry (or (alist-get 'anyDepthDirectories dotfiles-test/sensitive-paths)
                     (error "The sensitive path list has no any-depth directories")))
    (dolist (root (list (expand-file-name "~") elsewhere
                        (expand-file-name "~/project")
                        (expand-file-name "deep/tree" elsewhere)))
      (let ((directory (expand-file-name entry root)))
        (dotfiles-test/check-sensitive-state
         (expand-file-name "credential.txt" directory))
        (dotfiles-test/check-sensitive-state
         (expand-file-name "nested/credential.txt" directory))
        (dotfiles-test/check-ordinary
         (expand-file-name "ordinary.txt" (concat directory "-notes")))))))

;; The other local entries are anchored: a directory of the same name
;; elsewhere, such as a project's own .claude or .config, keeps its state.
(dolist (entry (dotfiles-test/entries 'home 'directories))
  (dotfiles-test/check-ordinary
   (expand-file-name (concat "project/" entry "/ordinary.txt") "~")))
(dolist (entry (dotfiles-test/entries 'xdgConfig 'directories))
  (dotfiles-test/check-ordinary
   (expand-file-name (concat "project/.config/" entry "/ordinary.txt") "~")))
(let ((project-file (expand-file-name "~/project/.claude/ordinary.txt")))
  (make-directory (file-name-directory project-file) t)
  (with-temp-file project-file
    (insert "initial fixture\n"))
  (dotfiles-test/exercise-file-state project-file nil))

;; A remote home is unknown without a connection, so a remote name is
;; guarded wherever an entry appears in it, and matching opens no connection.
(cl-letf (((symbol-function 'tramp-file-name-handler)
           (lambda (operation &rest _)
             (error "Unexpected remote file operation: %s" operation))))
  (dolist (entry (dotfiles-test/entries 'home 'files))
    (dotfiles-test/check-sensitive (concat "/ssh:host.invalid:/srv/user/" entry)))
  (dolist (entry (append (alist-get 'anyDepthDirectories dotfiles-test/sensitive-paths)
                         (dotfiles-test/entries 'home 'directories)))
    (dotfiles-test/check-sensitive
     (concat "/ssh:host.invalid:/srv/user/" entry "/credential.txt")))
  (dolist (entry (dotfiles-test/entries 'xdgConfig 'files))
    (dotfiles-test/check-sensitive
     (concat "/ssh:host.invalid:/srv/user/.config/" entry)))
  (dolist (entry (dotfiles-test/entries 'xdgConfig 'directories))
    (dotfiles-test/check-sensitive
     (concat "/ssh:host.invalid:/srv/user/.config/" entry "/credential.txt")))
  (dotfiles-test/check-ordinary "/ssh:host.invalid:/srv/user/ordinary.txt"))

;; A relative XDG_CONFIG_HOME names no config directory.
(let ((process-environment
       (cons "XDG_CONFIG_HOME=relative-config" process-environment)))
  (unless (equal (dotfiles/xdg-config-roots)
                 (list (expand-file-name "~/.config")))
    (error "A relative XDG_CONFIG_HOME was taken as a config directory")))

;; Symlinks, renamed buffers, and major-mode changes, for one directory
;; entry under the relocated root.
(let* ((secret-dir (expand-file-name "rclone" dotfiles-test/config-home))
       (secret (expand-file-name "credential.txt" secret-dir))
       (alias (expand-file-name "~/secret-alias.txt"))
       (directory-alias (expand-file-name "~/rclone-alias"))
       (ordinary (expand-file-name "~/ordinary.txt"))
       (renamed (expand-file-name "~/renamed.txt")))
  (make-directory secret-dir t)
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
