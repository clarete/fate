;;; fate.el --- Track what the gods told you to do on your computer
;;
;;; Commentary:
;;
;;; Code:

(defvar fate:data-file "~/.fate.db" "Path to save collected data.")

(defvar fate:last-buffer nil "Last buffer visited.")

(defvar fate:current-buffer (current-buffer) "Currently open buffer.")

(defun fate:update-state ()
  "Update `fate:last-buffer' and `fate:current-buffer'."
  (setq fate:last-buffer fate:current-buffer)
  (setq fate:current-buffer (current-buffer)))

(defun fate:buffer-string (buffer)
  "Return either path or name of BUFFER."
  (or (buffer-file-name buffer)
      (buffer-name buffer)))

(defun fate:state-string ()
  "Generate representation of the current state."
  (format "%s,%s,%s\n"
          (format-time-string "%FT%T.%N%z")
          (fate:buffer-string fate:last-buffer)
          (fate:buffer-string fate:current-buffer)))

(defun fate:log-state ()
  "Write the current state to the database file."
  (let ((inhibit-message t)
        (message-log-max nil))
    (write-region (fate:state-string) nil fate:data-file 'append)))

;; This lil bit of code was inspired on this link
;; https://stackoverflow.com/questions/47456134/emacs-lisp-hooks-for-detecting-change-of-active-buffer
(defun fate:post-command-watcher ()
  "Decide if internal state should be updated."
  (if (not (eq fate:current-buffer (current-buffer)))
      (progn
        (fate:update-state)
        (fate:log-state))))

(add-hook 'post-command-hook 'fate:post-command-watcher)

(provide 'fate)

;;; fate.el ends here
