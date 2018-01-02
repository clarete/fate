;;; fate.el --- Track what the gods told you to do on your computer
;;
;;; Commentary:
;;
;;; Code:

(defcustom fate:data-file "~/.fate.db"
  "Path to save collected data."
  :group 'fate
  :type 'string)

(defvar fate:last-buffer nil
  "Last buffer visited.")

(defvar fate:current-buffer (current-buffer)
  "Currently open buffer.")

(defcustom fate:idle-time 30
  "Log as idle time after N seconds."
  :group 'fate
  :type 'integer)

(defvar fate:idle-state nil
  "Idle state flag.")

(defvar fate:idle-handler nil
  "Handler of the idle timer.")

(defun fate:update-state ()
  "Update `fate:last-buffer' and `fate:current-buffer'."
  (setq fate:last-buffer fate:current-buffer)
  (setq fate:current-buffer (current-buffer)))

(defun fate:buffer-string (buffer)
  "Return either path or name of BUFFER."
  (or (buffer-file-name buffer)
      (buffer-name buffer)))

(defun fate:state-string-base (left right)
  "Represent state using LEFT and RIGHT."
  (format "%s,%s,%s\n" (format-time-string "%FT%T.%N%z") left right))

(defun fate:state-string ()
  "State string with `fate:last-buffer' & `fate:current-buffer'."
  (fate:state-string-base
   (fate:buffer-string fate:last-buffer)
   (fate:buffer-string fate:current-buffer)))

(defun fate:idle-state-in-string ()
  "String representing entering idle state."
  (fate:state-string-base
   (fate:buffer-string fate:current-buffer)
   "**idle-fate**"))

(defun fate:idle-state-out-string ()
  "String representing leaving idle state."
  (fate:state-string-base
   "**idle-fate**"
   (fate:buffer-string fate:current-buffer)))

(defun fate:log-state (state)
  "Write STATE to the database file."
  (let ((inhibit-message t)
        (message-log-max nil))
    (write-region state nil fate:data-file 'append)))

(defun fate:timer-to-idle-state ()
  "Schedule recording idle state after `fate:idle-time'."
  (if (not (eq fate:idle-handler nil))
      (cancel-timer fate:idle-handler))
  (setq fate:idle-handler
        (run-at-time fate:idle-time nil
                     #'(lambda ()
                         (fate:log-state (fate:idle-state-in-string))
                         (setq fate:idle-state t)
                         (setq fate:idle-handler nil))))
  (if fate:idle-state
      (progn
        (setq fate:idle-state nil)
        (fate:log-state (fate:idle-state-out-string))))
  (setq fate:idle-state nil))

;; This lil bit of code was inspired on this link
;; https://stackoverflow.com/questions/47456134/emacs-lisp-hooks-for-detecting-change-of-active-buffer
(defun fate:post-command-watcher ()
  "Decide if internal state should be updated."
  (fate:timer-to-idle-state)
  (if (not (eq fate:current-buffer (current-buffer)))
      (progn
        (fate:update-state)
        (fate:log-state (fate:state-string)))))

(add-hook 'post-command-hook 'fate:post-command-watcher)

(provide 'fate)

;;; fate.el ends here
