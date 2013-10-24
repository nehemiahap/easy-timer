;;; Easy Timer.el --- Another timer package
;; Author: Nehemiah Paramore 
;; Version 1.0


;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This has the same functionality of tea-time.el, except that
;; it allows for the pausing and unpausing of timers. 
;; This package allows you to set countdown timers that notify you 
;; via the message buffer upon completion. Unlike the much more elegantly
;; coded tea-time.el, this package allows you to pause/resume timers.
;; Depending on the percieved necessity, I'm considering complicating 
;; matters even further by adding the ability to set buffer specific 
;; timers that only run when the buffer is focused. 

;;; Installation:
;; Add the following code to your .emacs
;; (require 'easy-timer)
;; optionally add
;; (setq easy-timer-default-time "valid time string or number")
;; If you set it to a number, the type defaults to minutes. 
;; Ex. "30 sec", "30 min"

(defvar easy-timer-default-time nil
  "Default time string. If unit not specified, 
then defaults to minutes."
  :group 'easy-timer)

(defvar easy-timer nil
  "No timer set : easy-timer => nil.
Timer is running : easy-timer => reference to the timer.
Timer is paused : easy-timer => a valid time string of the time left."
  :group 'easy-timer)

(defun set-easy-timer (opt &optional paused)
  "Simple call to run-at-time. Assumes a proper string is always passed."
  (progn
    (setq easy-timer (run-at-time 
		      opt
		      nil 
		      `(lambda () 
			 (progn
			   (setq easy-timer nil)
			   (read-minibuffer 
			    "Timer finished. press ENTER to exit."
			    nil)))) )
    (if paused
	(message "Timer paused with %s left" opt)
      (message "Timer set with %s left." opt))))

(defun easy-timer-pause-resume (&optional opt)
  "Pause or resume the task timer, depending on it's status."
  (case (easy-timer-status)
    ('None
     ;; consider getting rid of this for brevity. 
     (if (y-or-n-p "No timer set. Set new timer?")
	 (progn
	   (let ((opt (or (format-time opt)
			  (format-time easy-timer-default-time))))
	     (set-easy-timer opt)))))
    ('Paused (set-easy-timer easy-timer t))
    ('Running
     (let* ((rtime (decode-time (time-subtract
				 (timer--time easy-timer)
				 (current-time))))
	    (rsecs (nth 0 rtime))
	    (rmins (nth 1 rtime)))
       (progn 
	 (cancel-timer easy-timer)
	 (setq easy-timer (concat
			   (number-to-string rmins)
			   " min "
			   (number-to-string rsecs)
			   " sec")))))))
		    
(defun format-time (&optional opt)
  "Format the user input for a proper call to time."
  (if (numberp opt) 
      (concat (number-to-string opt) " sec")
    opt))      

(defun easy-timer-status ()
  "Return the status of the current-timer as a string."
  (cond ((not easy-timer) 'None)
	((stringp easy-timer) 'Paused) 
	(t 'Running)))

(defun easy-timer-display-time ()
  "Display the remaining time."
  (interactive)
  (case (easy-timer-status)
    ('None (message "No timer set."))
    ('Paused (message "Timer paused with %s left." easy-timer))
    ('Running 
     (let* ((rtime (decode-time
		    (time-subtract (timer--time easy-timer)
				   (current-time))))
	    (rsecs (nth 0 rtime))
	    (rmins (nth 1 rtime)))
       (message "%d minute(s) %d seconds left before time out"
	      rmins rsecs)))))

(provide 'easy-timer)
;;; easy-timer.el ends here
