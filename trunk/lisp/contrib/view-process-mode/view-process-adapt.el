;;; $Id: view-process-adapt.el,v 1.1 1996/09/04 22:22:11 raman Exp raman $
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

(if (string< emacs-version "19.29")
    (defun font-lock-hack-keywords (start end &optional loudly)
      (goto-char start)
      (let ((case-fold-search font-lock-keywords-case-fold-search)
	    (rest font-lock-keywords)
	    (count 0)
	    (buffer-read-only nil)
	    (modified (buffer-modified-p))
	    first str match face s e allow-overlap-p
	    (old-syntax (syntax-table)))
	(unwind-protect
	    (progn
	      (if font-lock-syntax-table
		  (set-syntax-table font-lock-syntax-table))
	      (while rest
		(setq first (car rest) rest (cdr rest))
		(goto-char start)
		(cond ((consp first)
		       (setq str (car first))
		       (cond ((consp (cdr first))
			      (setq match (nth 1 first)
				    face (eval (nth 2 first))
				    allow-overlap-p (nth 3 first)))
			     ((symbolp (cdr first))
			      (setq match 0 allow-overlap-p nil
				    ;; face (eval (cdr first))))
				    face (cdr first)))
			     (t
			      (setq match (cdr first)
				    allow-overlap-p nil
				    face font-lock-keyword-face))))
		      (t
		       (setq str first match 0 allow-overlap-p nil
			     face font-lock-keyword-face)))
					;(message "regexp: %s" str)
		(while (re-search-forward str end t)
		  (setq s (match-beginning match)
		    e (match-end match))
		  (or s (error 
			 "expression did not match subexpression %d" match))
		  ;; don't fontify this keyword 
		  ;; if we're already in some other context.
		  (or (if allow-overlap-p nil (font-lock-any-properties-p s e))
		      (if (not (memq allow-overlap-p '(t nil)))
			  (save-excursion
			    (goto-char s)
			    (while (< (point) e)
			      (let ((next 
				     (next-single-property-change (point) 
								  'face
								  nil 
								  e)))
				(if (or (null next) (> next e))
				    (setq next e))
				(if (not (get-text-property (point) 'face))
				    (put-text-property (point) 
						       next 
						       'face face))
				(goto-char next))))
			(put-text-property s e 'face face))))
		(if loudly (message "Fontifying %s... (regexps...%s)"
				    (buffer-name)
				    (make-string 
				     (setq count (1+ count)) ?.)))))
	  (set-syntax-table old-syntax))
	(and (buffer-modified-p)
	     (not modified)
	     (set-buffer-modified-p nil))))
  )
