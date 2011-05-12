;;; $Id: view-process-xemacs.el,v 1.26 1996/08/17 14:22:09 muenkel Exp $
;;;
;;; Copyright (C) 1995, 1996 Heiko Muenkel
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
;;;	This file contains lisp code, which works only in the XEmacs.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load directories.
;;;

(provide 'view-process-xemacs)

;;; variables

(defvar View-process-itimer-name "view-process"
  "Name of the view process itimer.")


;;; special keybindings

(define-key View-process-mode-map '(button2) 'View-process-mouse-kill)
(define-key View-process-mode-map '(button3) 'View-process-popup-menu)


;;; menus

(if (not View-process-pulldown-menu)
    (setq
     View-process-pulldown-menu
     '("View-process-pulldown-menu-name"
       ["Rename Buffer" View-process-rename-current-output-buffer t]
       ["Submit Bug Report" View-process-submit-bug-report t]
       ["Quit" View-process-quit t]
       ("Options"
	["Truncate Lines" 
	 View-process-toggle-truncate-lines 
	 :style toggle
	 :selected truncate-lines]
	["Motion Help"
	 View-process-toggle-motion-help
	 :style toggle
	 :selected View-process-motion-help]
	["Two Windows"
	 View-process-toggle-display-with-2-windows
	 :style toggle
	 :selected View-process-display-with-2-windows]
	["Hide Header"
	 View-process-toggle-hide-header
	 :style toggle
	 :selected View-process-hide-header
	 :active View-process-display-with-2-windows]
	["Digits Send Signals"
	 View-process-toggle-digit-bindings
	 :style toggle
	 :selected View-process-digit-bindings-send-signal]
	)
       )))


(if (not View-process-region-menu)
    (setq 
     View-process-region-menu
     '("PS Region Menu"
       ["View Processes" view-processes nil]
       ["New PS" View-process-status nil]
       ["Update" View-process-status-update nil]
       ("Periodic Output"
	["Start " 
	 View-process-start-itimer 
	 :style radio 
	 :selected (not (get-itimer View-process-itimer-name))
	 :active nil]	
	["Stop" 
	 View-process-delete-itimer 
	 :style radio 
	 :selected (get-itimer View-process-itimer-name)
	 :active nil]
	)
       ("Send Signal"
	["SIGHUP" 
	 (View-process-send-signal-to-processes-in-region "SIGHUP") t]
	["SIGTERM" 
	 (View-process-send-signal-to-processes-in-region "SIGTERM") t]
	["SIGKILL" 
	 (View-process-send-signal-to-processes-in-region "SIGKILL") t]
	["SIGSTOP" 
	 (View-process-send-signal-to-processes-in-region "SIGSTOP") t]
	["SIGCONT" 
	 (View-process-send-signal-to-processes-in-region "SIGCONT") t]
	["SIGQUIT" 
	 (View-process-send-signal-to-processes-in-region "SIGQUIT") t]
	"----"
	["Any Signal..." View-process-send-signal-to-processes-in-region t]
	"----"
	["Alter Priority..." View-process-renice-processes-in-region t]
	)
       ("Mark"
	["Mark" View-process-mark-current-line nil]
	["Mark Childs" View-process-mark-childs-in-current-line nil]
	["Remark Last Marks" View-process-reset-last-marks nil]
	"----"
	["Unmark" View-process-unmark-current-line nil]
	["Unmark All" View-process-unmark-all nil]
	)
       "----"
       ["Sort" View-process-sort-region-by-current-field (looking-at "[^ ]")]
       ["Reverse" View-process-reverse-region t]
       ["Field Filter..." 
	View-process-filter-region-by-current-field 
	(looking-at "[^ ]")]
       ["Exlude Field Filter..." 
	(progn (setq current-prefix-arg '(-1))
	       (call-interactively 
		'View-process-filter-region-by-current-field))
	:keys "C-u -1 M-c f"
	:active (looking-at "[^ ]")]	    
       ["Line Filter..." View-process-filter-region t]
       ["Exclude Line Filter..." 
	(progn (setq current-prefix-arg '(-1))
	       (call-interactively 
		'View-process-filter-region))
	:keys "C-u -1 M-c g"
	:active t]
       "----"
       ("Help"
	["PID and Command" View-process-show-pid-and-command nil]
	["Field Name" View-process-which-field-name nil]
	["Header Line" View-process-show-header-line nil]
	["Own PID" View-process-display-emacs-pid nil]
	)
       )
     )
  )

(if (not View-process-marked-menu)
    (setq 
     View-process-marked-menu
     '("PS Marked Menu"
       ["View Processes" view-processes t]
       ["New PS" View-process-status t]
       ["Update" View-process-status-update t]
       ("Periodic Output"
	["Start " 
	 View-process-start-itimer 
	 :style radio 
	 :selected (not (get-itimer View-process-itimer-name))
	 :active nil]
	["Stop" 
	 View-process-delete-itimer 
	 :style radio 
	 :selected (get-itimer View-process-itimer-name)
	 :active nil]
	)
       ("Send Signal"
	["SIGHUP" (View-process-send-signal-to-processes-with-mark "SIGHUP") t]
	["SIGTERM" 
	 (View-process-send-signal-to-processes-with-mark "SIGTERM") 
	 t]
	["SIGKILL" 
	 (View-process-send-signal-to-processes-with-mark "SIGKILL") 
	 t]
	["SIGSTOP" 
	 (View-process-send-signal-to-processes-with-mark "SIGSTOP") 
	 t]
	["SIGCONT" 
	 (View-process-send-signal-to-processes-with-mark "SIGCONT") 
	 t]
	["SIGQUIT" 
	 (View-process-send-signal-to-processes-with-mark "SIGQUIT") 
	 t]
	"----"
	["Any Signal..." View-process-send-signal-to-processes-with-mark t]
	"----"
	["Alter Priority..." View-process-renice-processes-with-mark t]
	)
       ("Mark"
	["Mark" View-process-mark-current-line t]
	["Mark Childs" View-process-mark-childs-in-current-line t]
	["Remark Last Marks" View-process-reset-last-marks t]
	"----"
	["Unmark" View-process-unmark-current-line t]
	["Unmark All" View-process-unmark-all t]
	)
       "----"
       ["Sort" View-process-sort-output-by-current-field (looking-at "[^ ]")]
       ["Reverse" View-process-reverse-output t]
       ["Field Filter..." 
	View-process-filter-output-by-current-field (looking-at "[^ ]")]
       ["Exlude Field Filter..." 
	(progn (setq current-prefix-arg '(-1))
	       (call-interactively 
		'View-process-filter-output-by-current-field))
	:keys "C-u -1 F"
	:active (looking-at "[^ ]")]	    
       ["Line Filter..." View-process-filter-output t]
       ["Exclude Line Filter..." 
	(progn (setq current-prefix-arg '(-1))
	       (call-interactively 
		'View-process-filter-output))
	:keys "C-u -1 G"
	:active t]	    
       "----"
       ("Help"
	["PID and Command" View-process-show-pid-and-command t]
	["Field Name" View-process-which-field-name (looking-at "[^ ]")]
	["Header Line" View-process-show-header-line t]
	["Own PID" View-process-display-emacs-pid t]
	)
       )
     )
  )

(if (not View-process-non-region-menu)
    (setq 
     View-process-non-region-menu
     '("PS Non Region Menu"
       ["View Processes" view-processes t]
       ["New PS" View-process-status t]
       ["Update" View-process-status-update t]
       ("Periodic Output"
	["Start " 
	 View-process-start-itimer 
	 :style radio 
	 :selected (not (get-itimer View-process-itimer-name))]
	["Stop" 
	 View-process-delete-itimer 
	 :style radio 
	 :selected (get-itimer View-process-itimer-name)]
	)
       ("Send Signal"
	["SIGHUP" (View-process-send-signal-to-process-in-line "SIGHUP") t]
	["SIGTERM" (View-process-send-signal-to-process-in-line "SIGTERM") t]
	["SIGKILL" (View-process-send-signal-to-process-in-line "SIGKILL") t]
	["SIGSTOP" (View-process-send-signal-to-process-in-line "SIGSTOP") t]
	["SIGCONT" (View-process-send-signal-to-process-in-line "SIGCONT") t]
	["SIGQUIT" (View-process-send-signal-to-process-in-line "SIGQUIT") t]
	"----"
	["Any Signal..." View-process-send-signal-to-process-in-line t]
	"----"
	["Alter Priority..." View-process-renice-process-in-line t]
	)
       ("Mark"
	["Mark" View-process-mark-current-line t]
	["Mark Childs" View-process-mark-childs-in-current-line t]
	["Remark Last Marks" View-process-reset-last-marks t]
	"----"
	["Unmark" View-process-unmark-current-line nil]
	["Unmark All" View-process-unmark-all nil]
	)
       "----"
       ["Sort" View-process-sort-output-by-current-field (looking-at "[^ ]")]
       ["Reverse" View-process-reverse-output t]
       ["Field Filter..." 
	View-process-filter-output-by-current-field 
	(looking-at "[^ ]")]
       ["Exlude Field Filter..." 
	(progn (setq current-prefix-arg '(-1))
	       (call-interactively 
		'View-process-filter-output-by-current-field))
	:keys "C-u -1 F"
	:active (looking-at "[^ ]")]	    
       ["Line Filter..." View-process-filter-output t]
       ["Exclude Line Filter..." 
	(progn (setq current-prefix-arg '(-1))
	       (call-interactively 
		'View-process-filter-output))
	:keys "C-u -1 G"
	:active t]	    
       "----"
       ("Help"
	["PID and Command" View-process-show-pid-and-command t]
	["Field Name" View-process-which-field-name (looking-at "[^ ]")]
	["Header Line" View-process-show-header-line t]
	["Own PID" View-process-display-emacs-pid t]
	)
       )
     )
  )

(defun View-process-popup-menu (event)
  "Pops up a menu for the `View-process-mode'."
  (interactive "e")
  (mouse-set-point event)
  (popup-menu
   (cond ((View-process-region-active-p) View-process-region-menu)
	 (View-process-pid-mark-alist View-process-marked-menu)
	 (t View-process-non-region-menu))))

(defun View-process-install-pulldown-menu ()
  "Installs a pulldown menu for the `View-process-mode'."
  (if (and current-menubar 
	   (not (assoc View-process-pulldown-menu-name current-menubar)))
      (progn
	(set-buffer-menubar (copy-sequence current-menubar))
	(add-submenu nil
		     (cons View-process-pulldown-menu-name
			   (cdr View-process-pulldown-menu)))
	(add-submenu (list View-process-pulldown-menu-name)
		     View-process-region-menu
		     "Submit Bug Report")
	(add-submenu (list View-process-pulldown-menu-name)
		     View-process-marked-menu
		     "Submit Bug Report")
	(add-submenu (list View-process-pulldown-menu-name)
		     View-process-non-region-menu
		     "Submit Bug Report")
	)))


;;; mode motion

(defun View-process-mode-motion-highlight-line (event)
  "For use as the value of `mode-motion-hook' in the `View-process-mode'.
It highlights the line under the mouse and displays help messages during
mouse motion, if `View-process-motion-help' is non nil."
  (if (and (event-point event)
	   (> (event-point event) View-process-header-end))
      (progn
	(mode-motion-highlight-line event)
	(if (and View-process-motion-help
		 (not View-process-stop-motion-help))
	    (save-excursion
		(mouse-set-point event)
		(View-process-show-pid-and-command-or-field-name)
		)))
    (message "")
    ))

(defun View-process-install-mode-motion ()
  "Installs the `mode-motion-hook'."
  (make-local-variable 'mode-motion-hook)
  (setq mode-motion-hook 'View-process-mode-motion-highlight-line))

(defun View-process-toggle-motion-help (&optional arg)
  "Change whether a help message is displayed during mouse motion.
With a positive ARG the variable 'View-process-motion-help' is set
to t and with a negative ARG it is set to nil."
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 0)
	  (setq View-process-motion-help t)
	(setq View-process-motion-help nil))
    (if View-process-motion-help
	(setq View-process-motion-help nil)
      (setq View-process-motion-help t))))

; necessary for the Emacs 19
(defalias 'View-process-insert-and-inherit 'insert)

;;; timer functions

(defun View-process-start-itimer ()
  "Starts or restarts the itimer for updating the process output."
  (interactive)
  (if (get-itimer View-process-itimer-name)
      (progn 
	(set-itimer-value (get-itimer View-process-itimer-name) 
			  View-process-itimer-value)
	(set-itimer-restart (get-itimer View-process-itimer-name)
			    View-process-itimer-value))
    (start-itimer View-process-itimer-name
		  'View-process-status-itimer-function
		  View-process-itimer-value
		  View-process-itimer-value)))

(defun View-process-delete-itimer ()
  "Stops (deletes) the view process itimer."
  (interactive)
  (if (get-itimer View-process-itimer-name)
      (delete-itimer View-process-itimer-name)))


;;; region

(defun View-process-region-active-p ()
  "Returns t, if a region is active.
If `zmacs-regions' is nil, then this return always nil."
  (if zmacs-regions
      (mark)))


;;; Misc

(defun View-process-return-current-command-key-as-string ()
  "Returns the key, which invokes the current command as string."
  (events-to-keys (this-command-keys)))

(defun View-process-redraw ()
  "Dummy function. It does nothing in the XEmacs."
  )


;;; font-lock and colors

(defun View-process-install-font-lock ()
  "Installs the `font-lock-mode', if `View-process-use-font-lock' is t."
  (if View-process-use-font-lock
      (font-lock-mode 1)))

(if (not (fboundp 'valid-color-name-p))
    (defalias 'valid-color-name-p 'x-valid-color-name-p))

(defun View-process-search-color-in-color-list (color-list)
  "Searches a valid color in the COLOR-LIST."
  (cond ((not color-list) nil)
	((listp color-list)
	 (if (valid-color-name-p (car color-list))
	     (car color-list)
	   (View-process-search-color-in-color-list (cdr color-list))))))

(defun View-process-search-color (color)
  "It returns a color, which could be displayed by the window manager.
COLOR is either a string with a color or a list with possible
colors."
  (cond ((not color) nil)
	((stringp color)
	 (if (valid-color-name-p color) color nil))
	((listp color)
	 (View-process-search-color-in-color-list color))
	(t nil)))
  
;;; missing function window-pixel-edges in XEmacs < 19.12
;;; Attention: This emulation is only valid, to test if a value 
;;; is 0 or not.
(if (not (fboundp 'window-pixel-edges))
    (defalias 'window-pixel-edges 'window-edges))


;;; Modeline 

(if (fboundp 'set-specifier)

(defun view-process-switch-buffer-modeline (modeline-on)
  "Switches the current modeline on, if MODELINE-ON is t.
Otherwise the modeline is switched off."
  (set-specifier has-modeline-p (cons (current-buffer) modeline-on)))


(defun view-process-switch-buffer-modeline (modeline-on)
  "Dummy function. 
Sorry, the modeline can't be switched off in this emacs version.
You have to update at least to XEmacs 19.12."
  )

)