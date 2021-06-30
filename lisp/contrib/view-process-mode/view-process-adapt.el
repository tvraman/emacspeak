;;; $Id: view-process-adapt.el,v 1.1 1996/09/04 22:22:11 raman Exp raman $  -*- lexical-binding: t; -*-
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
;;;  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.
;;;
;;; 
;;; Description:
;;;
;;;	Functions to port XEmacs functions to GNU Emacs 19.
;;; 
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load directories.
;;;

(provide 'view-process-adapt)

(load-library "lucid")

(defvar original-read-string-function nil
  "Points to the original Emacs 19 function read-string.")

(if (not original-read-string-function)
    (fset 'original-read-string-function
	  (symbol-function 'read-string)))

;(defun read-string (prompt &optional initial-contents history)
;  "Return a string from the minibuffer, prompting with string PROMPT.
;If non-nil, optional second arg INITIAL-CONTENTS is a string to insert
;in the minibuffer before reading.
;Third arg HISTORY, if non-nil, specifies a history list."
;  (read-from-minibuffer prompt initial-contents nil nil history))

(if (not (fboundp 'emacs-pid))
    (defun emacs-pid ()
      "Return the process ID of Emacs, as an integer.
This is a dummy function for old versions of the Emacs 19.
You should install a new version, which has `emacs-pid' implemented."
      0)
  )

;;; extents and faces
(if (not (fboundp 'facep))
    (defun facep (object)
      "Whether OBJECT is a FACE.
It's only a dummy function in the Emacs 19, which returns always nil."
      nil)
  )

(if (not (fboundp 'make-extent))
    (defun make-extent (beg end &optional buffer)
      (make-overlay beg end buffer))
  )

(if (not (fboundp 'extent-start-position))
    (defun extent-start-position (extent)
      "Return start position of EXTENT, or nil if EXTENT is detached."
      (overlay-start extent))
  )

(if (not (fboundp 'set-extent-property))
    (defun set-extent-property (extent prop value)
      (if (eq prop 'duplicable)
	  (cond ((and value (not (overlay-get extent prop)))
		 ;; If becoming duplicable, 
		 ;; copy all overlayprops to text props.
		 (add-text-properties (overlay-start extent)
				      (overlay-end extent)
				      (overlay-properties extent)
				      (overlay-buffer extent)))
		;; If becoming no longer duplicable, remove these text props.
		((and (not value) (overlay-get extent prop))
		 (remove-text-properties (overlay-start extent)
					 (overlay-end extent)
					 (overlay-properties extent)
					 (overlay-buffer extent))))
	;; If extent is already duplicable, put this property
	;; on the text as well as on the overlay.
	(if (overlay-get extent 'duplicable)
	    (put-text-property  (overlay-start extent)
				(overlay-end extent)
				prop value (overlay-buffer extent))))
      (overlay-put extent prop value))
  )

(if (not (fboundp 'set-extent-face))      
    (defun set-extent-face (extent face)
      (set-extent-property extent 'face face))
  )
      
(if (not (fboundp 'delete-extent))
    (defun delete-extent (extent)
      (set-extent-property extent 'duplicable nil)
      (delete-overlay extent))
  )
      
(if (not (fboundp 'read-number))
    (defun read-number (prompt &optional integers-only)
      "Reads a number from the minibuffer."
      (interactive)
      (let ((error t)
	    (number nil))
	(if integers-only
	    (while error
	      (let ((input-string (read-string prompt)))
		(setq number (if (string= "" input-string)
				 nil
			       (read input-string)))
		(if (integerp number)
		    (setq error nil))))
	  (while error
	    (let ((input-string (read-string prompt)))
	      (setq number (if (string= "" input-string)
			       nil
			     (read input-string)))		
	      (if (numberp number)
		  (setq error nil)))))
	number))
  )


