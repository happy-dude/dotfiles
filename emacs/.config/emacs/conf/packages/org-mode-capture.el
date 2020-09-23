;; macOS globalcapture support

;; See https://www.reddit.com/r/emacs/comments/6lzyg2/heres_how_to_do_emacsclient_global_orgcapture/ and https://cestlaz.github.io/posts/using-emacs-24-capture-2/
;; Bind app to:
;; /usr/local/bin/emacs --daemon 2>&1 &
;; Bind quick action to:
;; /usr/local/bin/emacsclient -s $(lsof -c Emacs | grep server | tr -s " " | cut -d' ' -f 8) -c -ne "(make-capture-frame)" >/dev/null 2>&1 &

;; Quick-capture for macOS
;;      Use Automator to create an Application that runs a shell script starting emacs as a daemon
;;      Bind that Application as a login item on startup for the user
;;      Use Automator to create a Quick Action that runs a shell script that starts emacsclient binded to daemon with org-capture
;;      Bind that Quick Action to a keyboard shortcut in Keyboard settings under System Preferences

(defadvice org-capture-finalize
           (after delete-capture-frame activate)
           "Advise capture-finalize to close the frame"
           (if (equal "capture" (frame-parameter nil 'name))
               (delete-frame)))

(defadvice org-capture-destroy
           (after delete-capture-frame activate)
           "Advise capture-destroy to close the frame"
           (if (equal "capture" (frame-parameter nil 'name))
               (delete-frame)))

;; noflet
(add-to-list 'load-path "~/.config/emacs/plugins/emacs-noflet")
(require 'noflet)
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
          (org-capture)))
