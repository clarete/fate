;;; fate.el --- Track what the gods told you to do on your computer
;;
;; Copyright (C) 2018  Lincoln de Sousa <lincoln@clarete.li>
;;
;; Author: Lincoln de Sousa <lincoln@clarete.li
;; Keywords: calendar
;; Version: 0.0.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Track switching buffers in a log file.
;;
;;; Code:

(defcustom fate:data-file "~/.fate.db"
  "Path to save collected data."
  :group 'fate
  :type 'string)

(defvar fate:last-buffer nil
  "Last buffer visited.")

(defcustom fate:idle-time 30
  "Log as idle time after N seconds."
  :group 'fate
  :type 'integer)

(defvar fate:idle-state nil
  "Idle state flag.")

(defvar fate:idle-handler nil
  "Handler of the idle timer.")

(defun fate:buffer-string (buffer)
  "Return either path or name of BUFFER."
  (or (buffer-file-name buffer)
      (buffer-name buffer)))

(defvar fate:current-buffer (fate:buffer-string (current-buffer))
  "Currently open buffer.")

(defun fate:update-state ()
  "Update `fate:last-buffer' and `fate:current-buffer'."
  (setq fate:last-buffer fate:current-buffer)
  (setq fate:current-buffer (fate:buffer-string (current-buffer))))

(defun fate:escape (string)
  "Escape quote char (\") in STRING."
  (format "%s" (prin1-to-string string)))

(defun fate:state-string-base (left right)
  "Represent state using LEFT and RIGHT."
  (format "%s,%s,%s\n"
          (format-time-string "%FT%T.%N%z")
          (fate:escape left)
          (fate:escape right)))

(defun fate:state-string ()
  "State string with `fate:last-buffer' & `fate:current-buffer'."
  (fate:state-string-base fate:last-buffer fate:current-buffer))

(defun fate:idle-state-in-string ()
  "String representing entering idle state."
  (fate:state-string-base fate:current-buffer "**idle-fate**"))

(defun fate:idle-state-out-string ()
  "String representing leaving idle state."
  (fate:state-string-base "**idle-fate**" fate:current-buffer))

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
  (if (not (eq fate:current-buffer (fate:buffer-string (current-buffer))))
      (progn
        (fate:update-state)
        (fate:log-state (fate:state-string)))))

(add-hook 'post-command-hook 'fate:post-command-watcher)

(provide 'fate)

;;; fate.el ends here
