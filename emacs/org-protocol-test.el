;;; org-protocol-test.el --- Keep browser capture fields literal -*- lexical-binding: t; -*-

;; Loaded after init.el with the check's disposable HOME and XDG directories.
(require 'cl-lib)
(require 'org-protocol)

(defvar dotfiles-test/protocol-evaluated nil
  "Set by the %(...) payload if a capture evaluates it.")

(let* ((fixture (expand-file-name "~/protocol-fixture.txt"))
       (fixture-text "contents of the protocol fixture")
       (payloads (list "%(ignore (setq dotfiles-test/protocol-evaluated t))"
                       (format "%%[%s]" fixture))))
  (make-directory org-roam-directory t)
  (with-temp-file fixture
    (insert fixture-text))
  (dolist (field '(title body ref))
    (dolist (payload payloads)
      (let* ((case-name (format "%s %d" field (cl-position payload payloads)))
             (title (if (eq field 'title) (concat "Case " payload) case-name))
             (body (if (eq field 'body) payload ""))
             (ref (concat "https://example.com/" (string-replace " " "-" case-name)
                          (if (eq field 'ref) (concat "?q=" payload) ""))))
        (org-protocol-check-filename-for-protocol
         (concat "org-protocol://roam-ref?template=r"
                 "&ref=" (url-hexify-string ref)
                 "&title=" (url-hexify-string title)
                 "&body=" (url-hexify-string body))
         nil nil)
        ;; The protocol advice names this batch frame "capture"; keep it.
        (cl-letf (((symbol-function 'delete-frame) #'ignore))
          (with-current-buffer (cl-find-if (lambda (buffer)
                                             (buffer-local-value 'org-capture-mode buffer))
                                           (buffer-list))
            (org-capture-finalize)))
        (when dotfiles-test/protocol-evaluated
          (error "Protocol %s evaluated its payload" case-name))
        (with-temp-buffer
          (insert-file-contents
           (car (directory-files org-roam-directory t
                                 (concat (org-roam-node-slug
                                          (org-roam-node-create :title title))
                                         "\\.org\\'"))))
          (when (string-search fixture-text (buffer-string))
            (error "Protocol %s inserted the fixture file" case-name))
          (unless (string-search (concat "#+TITLE: " title "\n") (buffer-string))
            (error "Protocol %s changed the title" case-name))
          (unless (string-search (concat "#+begin_quote\n" body "\n#+end_quote")
                                 (buffer-string))
            (error "Protocol %s changed the body" case-name))
          (org-mode)
          (unless (member ref (split-string-and-unquote
                               (org-entry-get (point-min) "ROAM_REFS")))
            (error "Protocol %s changed the ref" case-name)))))))

;;; org-protocol-test.el ends here
