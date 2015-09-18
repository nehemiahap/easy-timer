;;; easy-timer.el --- another countdown timer package
;; Author: Nehemiah Paramore
;; Version 1.1
;; Additional code, Evans Winner

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This has the same functionality of tea-time.el, except that it
;; allows for the pausing and unpausing of timers.  This package
;; allows you to set countdown timers that notify you via the message
;; buffer upon completion.  Unlike tea-time.el, this package allows
;; you to pause/resume timers and use more flexible time values.

;;; Installation:

;; Put the library somewhere in your load path, and then add the
;; following code to your .emacs:

;;    (require 'easy-timer)

;; Optionally add

;; (setq easy-timer-default-time "valid time string or number")

;; If you set it to a number, the type defaults to minutes.  Ex. "30
;; sec", "30 min".

;; Use M-x customize-group RET easy-timer RET to enter the
;; customization.  You can add functions you write to
;; `easy-timer-alarm-hooks' to make anything you want happen when the
;; timer runs out; for example: `easy-timer-obnoxious-flash'.

(defgroup easy-timer nil
  "Customization group for Easy Timer.")

(defcustom easy-timer-default-time "3 min" ;tea
  "Default time string.  If no unit specified, default to minutes."
  :type 'string
  :group 'easy-timer)

(defcustom easy-timer-alarm-hooks '(easy-timer-default-alarm-function)
  "User functions to run when the time runs out."
  :type 'hook
  :options '(easy-timer-default-alarm-function ding easy-timer-obnoxious-flash)
  :group 'easy-timer)

(defvar easy-timer-data nil
  "Set programatically.
nil if no timer is set; a reference to the timer if the timer is
running; a time string of the remaining time if the timer is
paused.")

(defun easy-timer-default-alarm-function ()
  "Default action to take when the timer expires."
  (read-minibuffer "Timer finished. Press RET to acknowledge." nil))

(defun easy-timer-obnoxious-flash ()
  "Rapidly cycle background through several colors."
  (let ((colors '("black" "blue" "green" "cyan" "red" "magenta" "brown"))
	(original-color (frame-parameter nil 'background-color)))
    (dolist (color colors)
      (progn
	(set-background-color color)
	(redisplay)))
    (set-background-color original-color)))

(defun easy-timer-set-timer (time-str &optional paused)
  "Run a timer.  Requires a proper time string."
  (setq
   easy-timer-data
   (run-at-time
    time-str nil
    (lambda ()
      (setq easy-timer-data nil)
      (run-hooks 'easy-timer-alarm-hooks))))
  (if paused
      (message "Timer paused with %s left" time-str)
    (message "Timer set with %s left." time-str)))

(defun easy-timer (&optional arg)
  "Run Easy Timer interactively for `easy-timer-default-time'.
With prefix argument, query for a time as an Emacs standard
format time string.  Cancels and overrides an existing timer if
one is running."
  (interactive "P")
  (let ((time (if arg (read-string "Countown timer for how long? ")
		easy-timer-default-time)))
    (setq easy-timer-data nil)
    (easy-timer-set-timer time)))

(defun easy-timer-pause (&optional time)
  "Pause or resume the task timer, depending on it's status."
  (interactive)
  (case (easy-timer-status)
    ('none (message "No timer currently set"))
    ('paused (progn
	       (easy-timer-set-timer easy-timer-data t)
	       (easy-timer-display-time)))
    ('running
     (let* ((rtime (decode-time (time-subtract
				 (timer--time easy-timer-data)
				 (current-time))))
	    (rsecs (nth 0 rtime))
	    (rmins (nth 1 rtime)))
       (cancel-timer easy-timer-data)
       (setq easy-timer-data (concat
				(number-to-string rmins)
				" min "
				(number-to-string rsecs)
				" sec"))
       (easy-timer-display-time)))))

(defun easy-timer-status ()
  "Return the status of the current-timer as a symbol."
  (interactive)
  (cond ((not easy-timer-data) 'none)
	((stringp easy-timer-data) 'paused)
	(t 'running)))

(defun easy-timer-display-time ()
  "Display the remaining time."
  (interactive)
  (case (easy-timer-status)
    ('none (message "No timer set."))
    ('paused (message "Timer paused with %s left." easy-timer-data))
    ('running
     (let* ((rtime (decode-time
		    (time-subtract (timer--time easy-timer-data)
				   (current-time))))
	    (rsecs (nth 0 rtime))
	    (rmins (nth 1 rtime)))
       (message "%d minute(s) %d second(s) left before time out."
	      rmins rsecs)))))

(provide 'easy-timer)
;;; easy-timer.el ends here
