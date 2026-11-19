;;; org-protocol-test.el --- Keep browser capture fields literal -*- lexical-binding: t; -*-

;; Loaded after init.el with the check's disposable HOME and XDG directories.
(require 'cl-lib)
(require 'org-element)
(require 'org-protocol)

(defvar dotfiles-test/protocol-evaluated nil
  "Set by the %(...) payload if a capture evaluates it.")

(defconst dotfiles-test/note-headlines
  '("Summary" "Questions" "Impact" "Purpose (Inspire)" "Values (Guide)"
    "Habits (Define)" "Notes")
  "The headings of a new note, which are all a capture may leave in it.")

(defun dotfiles-test/protocol-open (title body ref)
  "Hand org-protocol a roam-ref URL for REF with TITLE and BODY."
  (org-protocol-check-filename-for-protocol
   (concat "org-protocol://roam-ref?template=r"
           "&ref=" (url-hexify-string ref)
           "&title=" (url-hexify-string title)
           "&body=" (url-hexify-string body))
   nil nil))

(defun dotfiles-test/capture-buffer ()
  "Return the buffer of a capture in progress, or nil."
  (cl-find-if (lambda (buffer)
                (buffer-local-value 'org-capture-mode buffer))
              (buffer-list)))

(defun dotfiles-test/protocol-capture (title body ref)
  "Capture REF with TITLE and BODY through org-protocol and finalize it."
  (dotfiles-test/protocol-open title body ref)
  ;; The protocol advice names this batch frame "capture"; keep it.
  (cl-letf (((symbol-function 'delete-frame) #'ignore))
    (with-current-buffer (dotfiles-test/capture-buffer)
      (org-capture-finalize)))
  (save-some-buffers t)
  (org-roam-db-sync))

(defun dotfiles-test/note-text (title)
  "Return the text of the note created for TITLE."
  (with-temp-buffer
    (insert-file-contents
     (car (directory-files org-roam-directory t
                           (concat (org-roam-node-slug
                                    (org-roam-node-create :title title))
                                   "\\.org\\'"))))
    (buffer-string)))

(defun dotfiles-test/one-line (text)
  "Return TEXT as the single line a #+TITLE keyword can hold."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " text)))

(let* ((fixture (expand-file-name "~/protocol-fixture.txt"))
       (fixture-text "contents of the protocol fixture")
       ;; The structure payloads come first, so a capture that lets page text
       ;; become Org structure fails on that rather than on a later case.
       (payloads
        (list
         ;; Page text that tries to end its block, in every spelling Org
         ;; accepts, and then to add headings and a source block.
         (concat "ordinary text\n"
                 "#+end_quote\n"
                 "#+end_example\n"
                 "  #+END_EXAMPLE\n"
                 "* Injected heading\n"
                 "** Injected child\n"
                 "#+begin_src sh\n"
                 "touch ~/protocol-ran\n"
                 "#+end_src\n"
                 ",* already escaped\n"
                 "#+begin_example")
         ;; An ordinary selection: a list copied from a page.
         "Three points:\n* first\n* second\n* third"
         "%(ignore (setq dotfiles-test/protocol-evaluated t))"
         (format "%%[%s]" fixture))))
  (make-directory org-roam-directory t)
  (with-temp-file fixture
    (insert fixture-text))
  (dolist (field '(title body ref))
    (dolist (payload payloads)
      ;; A ref is one URL; the multi-line payloads apply to the page's text.
      (unless (and (eq field 'ref) (string-search "\n" payload))
        (let* ((case-name (format "%s %d" field (cl-position payload payloads)))
               (title (if (eq field 'title) (concat "Case " payload) case-name))
               (body (if (eq field 'body) payload ""))
               (ref (concat "https://example.com/" (string-replace " " "-" case-name)
                            (if (eq field 'ref) (concat "?q=" payload) ""))))
          (dotfiles-test/protocol-capture title body ref)
          (when dotfiles-test/protocol-evaluated
            (error "Protocol %s evaluated its payload" case-name))
          (with-temp-buffer
            (insert (dotfiles-test/note-text title))
            (when (string-search fixture-text (buffer-string))
              (error "Protocol %s inserted the fixture file" case-name))
            (org-mode)
            (let* ((tree (org-element-parse-buffer))
                   (headlines (org-element-map tree 'headline
                                (lambda (headline)
                                  (org-element-property :raw-value headline))))
                   (blocks (org-element-map tree 'src-block
                             (lambda (block)
                               (org-element-property :language block))))
                   (quotes (org-element-map tree 'example-block
                             (lambda (block)
                               (org-element-property :value block)))))
              (unless (equal headlines dotfiles-test/note-headlines)
                (error "Protocol %s changed the note's headings: %S"
                       case-name headlines))
              (when blocks
                (error "Protocol %s added a source block: %S" case-name blocks))
              ;; Org reads the block back unescaped, so this is the page's
              ;; text as the browser sent it.
              (unless (equal quotes (list (concat body "\n")))
                (error "Protocol %s changed the body: %S" case-name quotes)))
            (unless (string-search
                     (concat "#+TITLE: " (dotfiles-test/one-line title) "\n")
                     (buffer-string))
              (error "Protocol %s changed the title" case-name))
            (unless (member ref (split-string-and-unquote
                                 (org-entry-get (point-min) "ROAM_REFS")))
              (error "Protocol %s changed the ref" case-name))))))))

;; Org Roam writes the ref onto the ROAM_REFS line as it arrives, so a ref
;; with a line break could close the property drawer and add headings.  It
;; is refused before Org Roam sees it: no capture starts and no note is
;; written.  So is any other control character.
(let ((notes (directory-files org-roam-directory nil "\\.org\\'")))
  (dolist (ref (list "https://example.com/a\n* Injected via ref\n:END:\n* Another"
                     ;; What the bookmarklet sends for a page whose own URL
                     ;; holds an encoded line break: the guard checks it too.
                     "https://example.com/g%0A:END:%0A* Injected"
                     "https://example.com/b\r:END:"
                     "https://example.com/c\td"
                     "https://example.com/e\0"
                     "https://example.com/f\177"))
    (condition-case nil
        (progn
          (dotfiles-test/protocol-open "Refused" "selection" ref)
          (error "Protocol accepted a ref with a control character: %S" ref))
      (user-error nil))
    (when (dotfiles-test/capture-buffer)
      (error "A refused ref left a capture in progress: %S" ref))
    (unless (equal notes (directory-files org-roam-directory nil "\\.org\\'"))
      (error "A refused ref wrote a note: %S" ref))))

;; The transport decodes each field once. A percent escape belonging to
;; the page is literal text, even when the reference index uses a decoded key.
(let* ((title "Query %20 and %25")
       (body "Literal %20, %26, %2F and %0A")
       (ref "https://example.com/search?q=a%26b&next=%2Fpath%20here")
       (distinct "https://example.com/search?q=a&b&next=%2Fpath%20here"))
  (dotfiles-test/protocol-capture title body ref)
  (dotfiles-test/protocol-capture "Different query" "other page" distinct)
  (dotfiles-test/protocol-capture "Repeat query" "same page" ref)
  (with-temp-buffer
    (insert (dotfiles-test/note-text title))
    (org-mode)
    (unless (and (equal (org-entry-get (point-min) "ROAM_REFS") ref)
                 (string-search (concat "#+TITLE: " title "\n") (buffer-string))
                 (string-search (concat "#+begin_example\n" body "\n") (buffer-string))
                 (string-search "same page" (buffer-string))
                 (not (string-search "other page" (buffer-string))))
      (error "Encoded ref, title, body or repeated capture changed:\n%s" (buffer-string))))
  (with-temp-buffer
    (insert (dotfiles-test/note-text "Different query"))
    (org-mode)
    (unless (and (equal (org-entry-get (point-min) "ROAM_REFS") distinct)
                 (string-search "other page" (buffer-string))
                 (not (string-search "same page" (buffer-string))))
      (error "Distinct query strings shared a note:\n%s" (buffer-string)))))

;; Existing notes may have Org bracket links in ROAM_REFS. Match their
;; targets, not the surrounding brackets or a description.
(dolist (style '(bare described))
  (let* ((title (format "Bracket %s" style))
         (ref (format "https://example.org/%s?q=a%%26b" style))
         (entry (if (eq style 'bare) (format "[[%s]]" ref)
                  (format "[[%s][description]]" ref)))
         (file (expand-file-name (concat (downcase (symbol-name style)) ".org")
                                 org-roam-directory)))
    (with-temp-file file
      (insert ":PROPERTIES:\n:ID: " (org-id-new) "\n:ROAM_REFS: " entry
              "\n:END:\n#+TITLE: " title "\n\n* Notes\n"))
    (org-roam-db-sync)
    (dotfiles-test/protocol-capture "Reuse bracket" "existing note" ref)
    (with-temp-buffer
      (insert-file-contents file)
      (unless (string-search "existing note" (buffer-string))
        (error "Capture did not reuse %s" entry)))))

;; A second capture of the same ref goes into the existing note, after the
;; first quote under its Notes heading.
(dotfiles-test/protocol-capture "Repeat" "first quote" "https://example.com/repeat")
(dotfiles-test/protocol-capture "Repeat" "second quote" "https://example.com/repeat")
(let ((note (dotfiles-test/note-text "Repeat")))
  (unless (string-match-p
           (concat "^\\* Notes\n\\(?:.*\n\\)*?#\\+begin_example\nfirst quote\n"
                   "\\(?:.*\n\\)*?#\\+begin_example\nsecond quote\n")
           note)
    (error "A repeat capture did not go under the note's Notes heading:\n%s"
           note)))

;;; org-protocol-test.el ends here
