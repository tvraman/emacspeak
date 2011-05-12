;;; $Id: view-process-emacs-19.el,v 1.26 1995/06/03 15:49:10 muenkel Exp $
;;;
;;; Copyright (C) 1995 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; 
;;; Description:
;;;
;;;	This file contains lisp code, which works only in the Emacs 19.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load directories.
;;;

(provide 'view-process-emacs-19)

;;; special keybindings

(define-key View-process-mode-map [mouse-2] 'View-process-mouse-kill)


;;; menus

(if (not View-process-region-menu)
    (progn


      ;; Commands for the Submenu "Send Signal"
      (defun View-process-send-signal-sighup-to-processes-in-region ()
	"Sends SIGHUP to all processes in the region."
	(interactive)
	(View-process-send-signal-to-processes-in-region "SIGHUP"))

      (defun View-process-send-signal-sigterm-to-processes-in-region ()
	"Sends SIGTERM to all processes in the region."
	(interactive)
	(View-process-send-signal-to-processes-in-region "SIGTERM"))

      (defun View-process-send-signal-sigkill-to-processes-in-region ()
	"Sends SIGKILL to all processes in the region."
	(interactive)
	(View-process-send-signal-to-processes-in-region "SIGKILL"))

      (defun View-process-send-signal-sigstop-to-processes-in-region ()
	"Sends SIGSTOP to all processes in the region."
	(interactive)
	(View-process-send-signal-to-processes-in-region "SIGSTOP"))

      (defun View-process-send-signal-sigcont-to-processes-in-region ()
	"Sends SIGCONT to all processes in the region."
	(interactive)
	(View-process-send-signal-to-processes-in-region "SIGCONT"))

       (defun View-process-send-signal-sigquit-to-processes-in-region ()
	"Sends SIGQUIT to all processes in the region."
	(interactive)
	(View-process-send-signal-to-processes-in-region "SIGQUIT"))


      ;; Submenu "Send Signal"
       
      (setq View-process-send-signal-region-menu
	    (make-sparse-keymap "Send Signal"))
      (define-key View-process-send-signal-region-menu
	[View-process-renice-processes-in-region]
	'("Alter Priority..." . View-process-renice-processes-in-region))
      (define-key View-process-send-signal-region-menu [seperator-region-2]
	'("--"))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-to-processes-in-region]
	'("Any Signal..." . View-process-send-signal-to-processes-in-region))
      (define-key View-process-send-signal-region-menu [seperator-region-1]
	'("--"))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-sigquit-to-processes-in-region]
	'("SIGQUIT" . View-process-send-signal-sigquit-to-processes-in-region))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-sigcont-to-processes-in-region]
	'("SIGCONT" . View-process-send-signal-sigcont-to-processes-in-region))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-sigstop-to-processes-in-region]
	'("SIGSTOP" . View-process-send-signal-sigstop-to-processes-in-region))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-sigkill-to-processes-in-region]
	'("SIGKILL" . View-process-send-signal-sigkill-to-processes-in-region))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-sigterm-to-processes-in-region]
	'("SIGTERM" . View-process-send-signal-sigterm-to-processes-in-region))
      (define-key View-process-send-signal-region-menu
	[View-process-send-signal-sighup-to-processes-in-region]
	'("SIGHUP" . View-process-send-signal-sighup-to-processes-in-region))


      (setq View-process-region-menu (make-sparse-keymap "PS Region Menu"))
      
;      (define-key View-process-region-menu [View-process-filter-region]
;      '("Exclude Line Filter..."
;      . View-process-filter-region))
      (define-key View-process-region-menu [View-process-filter-region]
	'("Line Filter..." . View-process-filter-region))
;      (define-key View-process-region-menu
;	[View-process-filter-region-by-current-field]
;	'("Exlude Field Filter..."
;	.  View-process-filter-region-by-current-field))
      (define-key View-process-region-menu
	[View-process-filter-region-by-current-field]
	'("Field Filter..." . View-process-filter-region-by-current-field))
      (define-key View-process-region-menu
	[View-process-reverse-region]
	'("Reverse" . View-process-reverse-region))
      (define-key View-process-region-menu 
	[View-process-sort-region-by-current-field]
	'("Sort" . View-process-sort-region-by-current-field))
      (define-key View-process-region-menu [seperator-region-1]
	'("--"))
      (define-key View-process-region-menu [send-signal]
	(cons "Send Signal" View-process-send-signal-region-menu))
      ))


(if (not View-process-non-region-menu)
    (progn

      ;; Submenu "Periodic Output"
      (setq View-process-periodic-output-menu
	    (make-sparse-keymap "Periodic Output"))
      (define-key View-process-periodic-output-menu 
	[View-process-delete-itimer]
	'("Stop" . View-process-delete-itimer))
      (define-key View-process-periodic-output-menu [View-process-start-itimer]
	'("Start" . View-process-start-itimer))


      ;; Commands for the Submenu "Send Signal"
      (defun View-process-send-signal-sighup-to-process-in-line ()
	"Sends SIGHUP to the process in the line."
	(interactive)
	(View-process-send-signal-to-process-in-line "SIGHUP"))

      (defun View-process-send-signal-sigterm-to-process-in-line ()
	"Sends SIGTERM to the process in the line."
	(interactive)
	(View-process-send-signal-to-process-in-line "SIGTERM"))

      (defun View-process-send-signal-sigkill-to-process-in-line ()
	"Sends SIGKILL to the process in the line."
	(interactive)
	(View-process-send-signal-to-process-in-line "SIGKILL"))

      (defun View-process-send-signal-sigstop-to-process-in-line ()
	"Sends SIGSTOP to the process in the line."
	(interactive)
	(View-process-send-signal-to-process-in-line "SIGSTOP"))

      (defun View-process-send-signal-sigcont-to-process-in-line ()
	"Sends SIGCONT to the process in the line."
	(interactive)
	(View-process-send-signal-to-process-in-line "SIGCONT"))

      (defun View-process-send-signal-sigquit-to-process-in-line ()
	"Sends SIGQUIT to the process in the line."
	(interactive)
	(View-process-send-signal-to-process-in-line "SIGQUIT"))

      ;; Submenu "Send Signal"
      (setq View-process-send-signal-non-region-menu
	    (make-sparse-keymap "Send Signal"))
      (define-key View-process-send-signal-non-region-menu
	[View-process-renice-process-in-line]
	'("Alter Priority..." . View-process-renice-process-in-line))
      (define-key View-process-send-signal-non-region-menu 
	[seperator-non-region-2]
	'("--"))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-to-process-in-line]
	'("Any Signal..." . View-process-send-signal-to-process-in-line))
      (define-key  View-process-send-signal-non-region-menu 
	[seperator-non-region-1]
	'("--"))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-sigquit-to-process-in-line]
	'("SIGQUIT" . View-process-send-signal-sigquit-to-process-in-line))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-sigcont-to-process-in-line]
	'("SIGCONT" . View-process-send-signal-sigcont-to-process-in-line))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-sigstop-to-process-in-line]
	'("SIGSTOP" . View-process-send-signal-sigstop-to-process-in-line))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-sigkill-to-process-in-line]
	'("SIGKILL" . View-process-send-signal-sigkill-to-process-in-line))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-sigterm-to-process-in-line]
	'("SIGTERM" . View-process-send-signal-sigterm-to-process-in-line))
      (define-key View-process-send-signal-non-region-menu
	[View-process-send-signal-sighup-to-process-in-line]
	'("SIGHUP" . View-process-send-signal-sighup-to-process-in-line))


      ;; Submenu "Mark"
      (setq View-process-non-region-mark-menu (make-sparse-keymap "Mark"))
      (define-key View-process-non-region-mark-menu 
	[View-process-reset-last-marks]
	'("Remark Last Marks" . View-process-reset-last-marks))
      (define-key View-process-non-region-mark-menu 
	[View-process-mark-childs-in-current-line]
	'("Mark Childs" . View-process-mark-childs-in-current-line))
      (define-key View-process-non-region-mark-menu 
	[View-process-mark-current-line]
	'("Mark" . View-process-mark-current-line))


      ;; Submenu "Help"
      (setq View-process-help-menu
	    (make-sparse-keymap "Help"))
      (define-key View-process-help-menu [View-process-display-emacs-pid]
	'("Own PID" . View-process-display-emacs-pid))
      (define-key View-process-help-menu [View-process-show-header-line]
	'("Header Line" . View-process-show-header-line))
      (define-key View-process-help-menu [View-process-which-field-name]
	'("Field Name" . View-process-which-field-name))
      (define-key View-process-help-menu [View-process-show-pid-and-command]
	'("PID and Command" . View-process-show-pid-and-command))

      (setq View-process-non-region-menu 
	    (make-sparse-keymap "PS Region Menu"))
      (define-key View-process-non-region-menu [help]
	(cons "Help" View-process-help-menu))
;      (define-key View-process-non-region-menu [
      (define-key View-process-non-region-menu [View-process-filter-output]
	'("Line Filter..." . View-process-filter-output))
;      (define-key View-process-non-region-menu
;	[View-process-filter-output]
;	'("Exlude Field Filter..." 
;	  View-process-filter-output))
      (define-key View-process-non-region-menu
	[View-process-filter-output-by-current-field]
	'("Field Filter..." . View-process-filter-output-by-current-field))
      (define-key View-process-non-region-menu
	[View-process-reverse-output]
	'("Reverse" . View-process-reverse-output))
      (define-key View-process-non-region-menu 
	[View-process-sort-output-by-current-field]
	'("Sort" . View-process-sort-output-by-current-field))
      (define-key View-process-non-region-menu [seperator-region-1]
	'("--"))
      (define-key View-process-non-region-menu [mark]
	(cons "Mark" View-process-non-region-mark-menu))
      (define-key View-process-non-region-menu [send-signal]
	(cons "Send Signal" View-process-send-signal-non-region-menu))
      (define-key View-process-non-region-menu [periodic-output]
	(cons "Periodic Output" View-process-periodic-output-menu))
      (define-key View-process-non-region-menu [View-process-status-update]
	'("Update" . View-process-status-update))
      (define-key View-process-non-region-menu [view-processes]
	'("View Processes" . view-processes))

      ))


(if (not View-process-marked-menu)
    (progn

      ;; Submenu "Mark"
      (setq View-process-marked-mark-menu (make-sparse-keymap "Mark"))
      (define-key View-process-marked-mark-menu
	[View-process-unmark-all]
	'("Unmark All" . View-process-unmark-all))
      (define-key View-process-marked-mark-menu
	[View-process-unmark-current-line]
	'("Unmark" . View-process-unmark-current-line))
      (define-key View-process-marked-mark-menu [seperator-mark-1]
	'("--"))
      (define-key View-process-marked-mark-menu 
	[View-process-reset-last-marks]
	'("Remark Last Marks" . View-process-reset-last-marks))
      (define-key View-process-marked-mark-menu 
	[View-process-mark-childs-in-current-line]
	'("Mark Childs" . View-process-mark-childs-in-current-line))
      (define-key View-process-marked-mark-menu 
	[View-process-mark-current-line]
	'("Mark" . View-process-mark-current-line))


      ;; Commands for the Submenu "Send Signal"
      (defun View-process-send-signal-sighup-to-processes-with-mark ()
	"Sends SIGHUP to the process in the line."
	(interactive)
	(View-process-send-signal-to-processes-with-mark "SIGHUP"))

      (defun View-process-send-signal-sigterm-to-processes-with-mark ()
	"Sends SIGTERM to the process in the line."
	(interactive)
	(View-process-send-signal-to-processes-with-mark "SIGTERM"))

      (defun View-process-send-signal-sigkill-to-processes-with-mark ()
	"Sends SIGKILL to the process in the line."
	(interactive)
	(View-process-send-signal-to-processes-with-mark "SIGKILL"))

      (defun View-process-send-signal-sigstop-to-processes-with-mark ()
	"Sends SIGSTOP to the process in the line."
	(interactive)
	(View-process-send-signal-to-processes-with-mark "SIGSTOP"))

      (defun View-process-send-signal-sigcont-to-processes-with-mark ()
	"Sends SIGCONT to the process in the line."
	(interactive)
	(View-process-send-signal-to-processes-with-mark "SIGCONT"))

      (defun View-process-send-signal-sigquit-to-processes-with-mark ()
	"Sends SIGQUIT to the process in the line."
	(interactive)
	(View-process-send-signal-to-processes-with-mark "SIGQUIT"))

      ;; Submenu "Send Signal"
      (setq View-process-send-signal-marked-menu
	    (make-sparse-keymap "Send Signal"))
      (define-key View-process-send-signal-marked-menu
	[View-process-renice-processes-with-mark]
	'("Alter Priority..." . View-process-renice-processes-with-mark))
      (define-key View-process-send-signal-marked-menu 
	[seperator-marked-2]
	'("--"))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-to-processes-with-mark]
	'("Any Signal..." . View-process-send-signal-to-processes-with-mark))
      (define-key  View-process-send-signal-marked-menu 
	[seperator-marked-1]
	'("--"))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-sigquit-to-processes-with-mark]
	'("SIGQUIT" . View-process-send-signal-sigquit-to-processes-with-mark))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-sigcont-to-processes-with-mark]
	'("SIGCONT" . View-process-send-signal-sigcont-to-processes-with-mark))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-sigstop-to-processes-with-mark]
	'("SIGSTOP" . View-process-send-signal-sigstop-to-processes-with-mark))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-sigkill-to-processes-with-mark]
	'("SIGKILL" . View-process-send-signal-sigkill-to-processes-with-mark))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-sigterm-to-processes-with-mark]
	'("SIGTERM" . View-process-send-signal-sigterm-to-processes-with-mark))
      (define-key View-process-send-signal-marked-menu
	[View-process-send-signal-sighup-to-processes-with-mark]
	'("SIGHUP" . View-process-send-signal-sighup-to-processes-with-mark))
      

      (setq View-process-marked-menu 
	    (make-sparse-keymap "PS Marked Menu"))
      (define-key View-process-marked-menu [help]
	(cons "Help" View-process-help-menu))
      (define-key View-process-marked-menu [View-process-filter-output]
	'("Line Filter..." . View-process-filter-output))
      (define-key View-process-marked-menu
	[View-process-filter-output-by-current-field]
	'("Field Filter..." . View-process-filter-output-by-current-field))
      (define-key View-process-marked-menu
	[View-process-reverse-output]
	'("Reverse" . View-process-reverse-output))
      (define-key View-process-marked-menu 
	[View-process-sort-output-by-current-field]
	'("Sort" . View-process-sort-output-by-current-field))
      (define-key View-process-marked-menu [seperator-region-1]
	'("--"))
      (define-key View-process-marked-menu [mark]
	(cons "Mark" View-process-marked-mark-menu))
      (define-key View-process-marked-menu [send-signal]
	(cons "Send Signal" View-process-send-signal-marked-menu))
      (define-key View-process-marked-menu [View-process-status-update]
	'("Update" . View-process-status-update))
      (define-key View-process-marked-menu [view-processes]
	'("View Processes" . view-processes))
      ))

(if (not View-process-pulldown-menu)
    (progn

      ;; Submenu "Options"
      (setq View-process-options-menu
	    (make-sparse-keymap "Options"))
      (define-key View-process-options-menu
	[View-process-toggle-digit-bindings]
	'("Toggle Digits Send Signals" . View-process-toggle-digit-bindings))
      (define-key View-process-options-menu
	[transient-mark-mode]
	'("Toggle Transient Mark" . transient-mark-mode))
      (define-key View-process-options-menu
	[View-process-toggle-hide-header]
	'("Toggle Hide Header" . View-process-toggle-hide-header))
      (define-key View-process-options-menu
	[View-process-toggle-display-with-2-windows]
	'("Toggle Two Windows" . View-process-toggle-display-with-2-windows))
      (define-key View-process-options-menu
	[View-process-toggle-motion-help]
	'("Toggle Motion Help" . View-process-toggle-motion-help))
      (define-key View-process-options-menu 
	[View-process-toggle-truncate-lines]
	'("Toggle Truncate Lines" . View-process-toggle-truncate-lines))

      (setq View-process-pulldown-menu 
	    (make-sparse-keymap View-process-pulldown-menu-name))
      (define-key View-process-pulldown-menu [options]
	(cons "Options" View-process-options-menu))
      (define-key View-process-pulldown-menu [quit]
	'("Quit" . View-process-quit))
      (define-key View-process-pulldown-menu [send-bug-report]
	'("Submit Bug Report" . View-process-submit-bug-report))
      (define-key View-process-pulldown-menu [ps-region-menu]
	(cons "PS Region Menu" View-process-region-menu))
      (define-key View-process-pulldown-menu [ps-marked-menu]
	(cons "PS Marked Menu" View-process-marked-menu))
      (define-key View-process-pulldown-menu [ps-non-region-menu]
	(cons "PS Non Region Menu" View-process-non-region-menu))
      (define-key View-process-pulldown-menu 
	[View-process-rename-current-output-buffer]
	'("Rename Buffer" . View-process-rename-current-output-buffer))

      ))

(define-key View-process-mode-map [menu-bar View-process-pulldown-menu]
  (cons View-process-pulldown-menu-name View-process-pulldown-menu))
 
;(define-key View-process-mode-map [down-mouse-3]  View-process-region-menu)
 
(defun View-process-popup-menu (event)
  "Pops up a menu for the `View-process-mode'."
  (interactive "e")
  (mouse-set-point event)
  (let* ((selection
          (x-popup-menu
           event
	   (cond ((View-process-region-active-p) View-process-region-menu)
		 (View-process-pid-mark-alist  View-process-marked-menu)
		 (t View-process-non-region-menu)))))
    (and selection (call-interactively 
		    (nth (1- (length selection)) selection)))))

(define-key View-process-mode-map [down-mouse-3] 'View-process-popup-menu)

 
;; enable/disable menus
(put 'View-process-sort-region-by-current-field 
     'menu-enable 
     '(looking-at "[^ ]"))
(put 'View-process-filter-region-by-current-field
     'menu-enable
     '(looking-at "[^ ]"))
(put 'View-process-delete-itimer
     'menu-enable
     'View-process-emacs19-timer)
(put 'View-process-start-itimer
     'menu-enable
     '(not View-process-emacs19-timer))
(put 'View-process-which-field-name
     'menu-enable
     '(looking-at "[^ ]"))
(put 'View-process-filter-output-by-current-field
     'menu-enable
     '(looking-at "[^ ]"))
(put 'View-process-sort-output-by-current-field
     'menu-enable
     '(looking-at "[^ ]"))

(defun View-process-install-pulldown-menu ()
	nil)


;;; mode motion

(defun View-process-mode-motion-highlight-line ()
  "It highlights the line under the mouse."
  (let ((read-only buffer-read-only))
    (setq buffer-read-only nil)
    (save-excursion
      (goto-char View-process-output-start)
      (while (< (point) View-process-output-end)
;	(overlay-put 
;	 (make-overlay (View-process-return-beginning-of-line)
;		       (View-process-return-end-of-line))
;	 'mouse-face 
;	 'highlight)
;	(add-text-properties (View-process-return-beginning-of-line)
;			     (View-process-return-end-of-line)
;			     (list 'mouse-face 'highlight))
	(put-text-property (View-process-return-beginning-of-line)
			   (View-process-return-end-of-line)
			   'mouse-face 'highlight)
	(forward-line 1)))
    (setq buffer-read-only read-only)))

(defun View-process-mouse-motion-help ()
  "Displays help messages during mouse motion."
;  (setq heiko View-process-stop-motion-help)
  (if (not View-process-motion-help)
      (remove-hook 'post-command-hook 'View-process-mouse-motion-help)
    (if (not View-process-stop-motion-help)
	(track-mouse
	 (let ((event nil)
	       (done nil))
	   (while (not done)
	     (setq event (read-event))
	     (if (mouse-movement-p event)
		 (if (eq (selected-window) (car (car (cdr event))))
		     (save-excursion
		       (mouse-set-point event)
		       (if (and (>= (point) View-process-output-start)
				(< (point) View-process-output-end))
			   (View-process-show-pid-and-command-or-field-name))
		       ))
	       (setq unread-command-events
		     (append (list event) unread-command-events))
	       (setq done t)))))
      )))

(defun View-process-install-mode-motion ()
  "Installs the mouse motion and and highlighting functions."
  (View-process-mode-motion-highlight-line)
  (make-variable-buffer-local 'post-command-hook)
  (add-hook 'post-command-hook 'View-process-mouse-motion-help))

(defun View-process-toggle-motion-help (&optional arg)
  "Change whether a help message is displayed during mouse motion.
With a positive ARG the variable 'View-process-motion-help' is set
to t and with a negative ARG it is set to nil."
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 0)
	  (progn
	    (setq View-process-motion-help t)
	    (setq-default View-process-motion-help t)
	    (add-hook 'post-command-hook 'View-process-mouse-motion-help))
	(setq View-process-motion-help nil)
	(setq-default View-process-motion-help t)
	(remove-hook 'post-command-hook 'View-process-mouse-motion-help))
    (if View-process-motion-help
	(progn
	  (setq View-process-motion-help nil)
	  (setq-default View-process-motion-help nil)
	  (remove-hook 'post-command-hook 'View-process-mouse-motion-help))
      (setq View-process-motion-help t)
      (setq-default View-process-motion-help t)
      (add-hook 'post-command-hook 'View-process-mouse-motion-help))))

(defalias 'View-process-insert-and-inherit 'insert-and-inherit)


;;; timer functions

(defvar View-process-emacs19-timer nil
  "Internal timer variable for the emacs 19.
This variable is buffer local.")

(make-variable-buffer-local 'View-process-emacs19-timer)


(defun View-process-start-itimer ()
  "Starts the itimer for updating the process output."
  (interactive)
  (setq View-process-emacs19-timer
	(run-at-time View-process-itimer-value
		     View-process-itimer-value
		     'View-process-status-itimer-function))
  )

(defun View-process-delete-itimer ()
  "Stops (deletes) the view process itimer."
  (interactive)
  (if View-process-emacs19-timer
      (progn
	(cancel-timer View-process-emacs19-timer)
	(setq View-process-emacs19-timer nil))))


;;; region

(defun View-process-region-active-p ()
  "Returns t, if a region is active.
If `transient-mark-mode' is nil, then this return always nil."
  (if transient-mark-mode
      mark-active))


;;; Misc

(defun View-process-return-current-command-key-as-string ()
  "Returns the key, which invokes the current command as string."
  (this-command-keys))

(defvar View-process-redraw t
  "Internal Variable. Don't change it. 
Look also at the function `View-process-redraw'.")

(defun View-process-redraw ()
  "Redraws the current frame, if `View-process-redraw' is t.
After that `View-process-redraw' is set to nil.
This fixes a bug in the Emacs 19."
  (if View-process-redraw
      (progn
	(redraw-frame (selected-frame))
	(setq View-process-redraw nil))))


;;; font-lock and colors

(defun View-process-install-font-lock ()
  "Installs the `font-lock-mode', if `View-process-use-font-lock' is t.
An additional condition is that `window-system' is non nil. 
This is necessary, because one can't use the `font-lock-mode' in the
Emacs 19, if it is running in a terminal :-(."
  (if (and View-process-use-font-lock window-system)
      (progn 
	(setq font-lock-keywords View-process-font-lock-keywords)
	(font-lock-mode 1))))

(defun View-process-search-color-in-color-list (color-list)
  "Searches a valid color in the COLOR-LIST."
  (cond ((not color-list) nil)
	((listp color-list)
	 (if (x-color-defined-p (car color-list))
	     (car color-list)
	   (View-process-search-color-in-color-list (cdr color-list))))))

(defun View-process-search-color (color)
  "It returns a color, which could be displayed by the window manager.
COLOR is either a string with a color or a list with possible
colors."
  (if (and (string= "x" window-system)
	   (x-display-color-p))
      (cond ((not color) nil)
	    ((stringp color)
	     (if (x-color-defined-p color) color nil))
	    ((listp color)
	     (View-process-search-color-in-color-list color))
	    (t nil))))


;;; missing function window-pixel-edges in XEmacs < 19.12
;;; Attention: This emulation is only valid, to test if a value 
;;; is 0 or not.
(if (not (fboundp 'window-pixel-edges))
    (defalias 'window-pixel-edges 'window-edges))

;;; Modeline 
(defun view-process-switch-buffer-modeline (modeline-on)
  "Dummy function. 
Sorry, the modeline can't be switched off in this emacs version.
You have to update at least to XEmacs 19.12."
  )
