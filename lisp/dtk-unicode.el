;;; dtk-unicode.el --- Pronounce more characters correctly

;; Copyright 2007 Lukas Loehrer
;;; TVR: Integrated into Emacspeak July 6, 2008
;;; Using patch from Lukas.
;;
;; Author: Lukas Loehrer <loehrerl |at| gmx.net>
;; Version: $Id$
;; Keywords: 
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; 
;;; This  Provides Unicode support to the speech layer.

;;; Code:

(require 'cl)
(require 'descr-text)

;;{{{Customization

(defgroup dtk-unicode
  nil
  "Customization group for dtk-unicode."
  :group 'emacspeak
  :prefix "dtk-unicode-")

(defcustom dtk-unicode-character-replacement-alist
  '(
	(?“ . "\"")
	(?” . "\"")
	(?⋆ . "*")
	(?‘ . "'")
	(?’ . "'")
	(?– . "--")
	(?— . "--")
	(?― . "----") ; horizontal bar
	(?‖ . "||")
	(?… . "...")
	(?• . "*") ; bullet
	(?™ . "TM") ; trademark
	)
  "Explicit replacements for some characters."
  :group 'dtk-unicode
  :type '(alist
		  :key-type (character :tag "character")
		   :value-type (string :tag "replacement"))
		   )

(defcustom dtk-unicode-name-transformation-rules-alist
  '(
	("^greek\\( small\\| capital\\)? letter \\(.*\\)$" .  (lambda (s) (match-string 2 s)))
	("\\(.*\\) sign$" . (lambda (s) (match-string 1 s)))
	)
  "Alist of character name transformation rules."
  :group 'dtk-unicode
  :type '(repeat (cons :value ("." . identity)
				 (regexp :tag "pattern")
				 (function :tag "transformation")))
  )

;;}}}

;;{{{Variables




(defvar dtk-unicode-untouched-charsets
  '(ascii latin-iso8859-1)
  "*Characters of these charsets are completely ignored by dtk-unicode-replace-chars.")

(defvar dtk-unicode-handlers
  '(dtk-unicode-user-table-handler dtk-unicode-full-table-handler)
  "List of functions which are called in in this order for replacing an unspeakable character.

A handler returns a non-nil value if the   replacement was successful, nil otherwise.")

;;}}}

;;{{{Helper functions

(defun dtk-unicode-charset-limits (charset)
  "Return rough lower and upper limits for character codes in CHARSET."
  (cond
   ((eq charset 'ascii)
	(list 0 127))
   ((eq charset 'eight-bit-control)
	(list 128 159))
   ((eq charset 'eight-bit-graphic)
	(list 160 255))
   (t
	(let* ((dim (charset-dimension charset))
		   (chars (charset-chars charset))
		   min max)
	  (if (eq chars 96)
		  (setq min 32 max 127)
		(setq min 33 max 126))
	  (list (make-char charset min min) (make-char charset max max))))))
	  
(defun dtk-unicode-build-skip-regexp ()
  "Construct regexp to match all but the characters in dtk-unicode-untouched-charsets."
  (format "[^%s]"
		  (loop for charset in dtk-unicode-untouched-charsets
				when (charsetp charset)
				concat (apply 'format "%c-%c" (dtk-unicode-charset-limits charset)))))

(defvar dtk-unicode-charset-filter-regexp
  (dtk-unicode-build-skip-regexp)
  "Regular exppression that matches characters not in dtk-unicode-untouched-charsets.")

(defun dtk-unicode-update-untouched-charsets (charsets)
  "Update list of charsets we will not touch."
  (setq dtk-unicode-untouched-charsets charsets)
  (setq dtk-unicode-charset-filter-regexp (dtk-unicode-build-skip-regexp)))

(defvar dtk-unicode-cache (make-hash-table)
  "Cache for unicode data lookups.")

(defadvice describe-char-unicode-data (around dtk-unicode pre act)
  "Cache result."
  (let* ((char (ad-get-arg 0))
		 (result (gethash char dtk-unicode-cache 'not-found)))
	(if (eq result 'not-found)
		(progn
		  ad-do-it
		  (puthash char ad-return-value dtk-unicode-cache))
	  (setq ad-return-value result))))
		  
(defun dtk-unicode-name-for-char (char)
  "Return unicode name for character CHAR.

nil if CHAR is not in Unicode."
  (let* ((unicode (encode-char char 'ucs))
		 (char-desc (and unicode (condition-case nil
									 (let ((emacspeak-speak-cue-errors nil))
									   (describe-char-unicode-data unicode))
								   (error nil)))))
	(when char-desc
	  (downcase (cadr (assoc "Name" char-desc))))))

(defsubst dtk-unicode-apply-name-transformation-rules (name)
  "Apply transformation rules in dtk-unicode-simplification-rules-alist to NAME."
  (funcall
   (or (assoc-default name dtk-unicode-name-transformation-rules-alist 'string-match)
	   'identity)
   name))

(defun dtk-unicode-uncustomize-char (char)
  "Delete custom replacement for CHAR.

When called interactively, CHAR defaults to the character after point."
  (interactive (list (following-char)))
  (setq dtk-unicode-character-replacement-alist
		(loop for elem in dtk-unicode-character-replacement-alist
			  unless (eq (car elem) char) collect elem)))

(defun dtk-unicode-customize-char (char replacement)
  "Add a custom replacement string for CHAR.

When called interactively, CHAR defaults to the character after point."
  (interactive
   (let ((char (following-char)))
	 (list char
		   (read-string
			(format "Replacement for %c (0x%x) from charset %s: " char char (char-charset char))))))
  (push (cons char replacement) dtk-unicode-character-replacement-alist))

;;}}}

;;{{{Character replacement handlers

(defun dtk-unicode-user-table-handler (char pos)
  "Return user defined replacement character if it exists."
  (let ((replacement (cdr (assq char dtk-unicode-character-replacement-alist))))
	replacement))

(defun dtk-unicode-full-table-handler (char pos)
  "Uses the unicode data file to find the name of CHAR."
  (let ((char-desc (dtk-unicode-name-for-char char)))
	(when char-desc
	  (format  " %s " (dtk-unicode-apply-name-transformation-rules char-desc)))))

;;}}}

;;{{{External interface

(defun dtk-unicode-full-name-for-char (char)
  "Return full name of CHAR.

This is meant to be used in places where the user asks for a detailed description of CHAR."
  (dtk-unicode-name-for-char char))

(defun dtk-unicode-short-name-for-char (char)
  "Return name of CHAR.

This is meant to be used in places where the user asks for a short description of CHAR."
  (if (memq char dtk-unicode-untouched-charsets)
	  (char-to-string char)
	(dtk-unicode-name-for-char char)))

(defun dtk-unicode-replace-chars (mode)
  "Replace unicode characters in current buffer with something more TTS friendly.

This is the main entry point for this module.
The argument MODE specifies the current punctuation mode.
Does nothing for unibyte buffers."
  (when enable-multibyte-characters
	(let ((inhibit-read-only t))
	  (goto-char (point-min))
	  (while (re-search-forward dtk-unicode-charset-filter-regexp  nil t)
		(let* ((pos (match-beginning 0))
			   (char (char-after pos))
			   (replacement (save-match-data (run-hook-with-args-until-success 'dtk-unicode-handlers char pos))))
		  (when replacement
			(let ((props (text-properties-at pos)))
			  (replace-match replacement t t nil)
			  (when props
				(set-text-properties pos (point) props)))))))))
				
(defun unicode-name-at (pos)
  (interactive "d")
  (let* ((char (char-after pos)))
	(message "%s" (downcase (or
							 (dtk-unicode-full-name-for-char char)
							 "Unknown character")))))

;;}}}

(provide 'dtk-unicode)

;;{{{  emacs local variables

;;; local variables:
;;; coding: utf-8
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

;;; dtk-unicode.el ends here
