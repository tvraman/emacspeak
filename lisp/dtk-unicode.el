;;; dtk-unicode.el --- Pronounce more characters correctly
;;{{{ Header: Lukas

;; Copyright 2007 Lukas Loehrer
;;; TVR: Integrated into Emacspeak July 6, 2008
;;; Using patch from Lukas.
;;
;; Author: Lukas Loehrer <loehrerl |at| gmx.net>
;; Version: $Id$
;; Keywords:  TTS, Unicode

;;}}}
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-07-06 10:18:30 -0700 (Sun, 06 Jul 2008) $ |
;;;  $Revision: 4670 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{ Introduction

;;; Commentary:

;; 
;;; This  Provides Unicode support to the speech layer.

;;; Code:

;;}}}
;;{{{ Preamble

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'descr-text)

;;}}}
;;{{{ Customizations

(defgroup dtk-unicode
  nil
  "Customization group for dtk-unicode."
  :group 'emacspeak
  :prefix "dtk-unicode-")

(defcustom dtk-unicode-character-replacement-alist
  '(
    (? . "-")                       ; START OF GUARDED AREA
    (?° . " degrees ")                  ; degree sign 
    (?“ . "\"")                         ;LEFT DOUBLE QUOTATION MARK
    (?” . "\"")                         ; RIGHT DOUBLE QUOTATION MARK
    (?⋆ . "*")                          ; STAR OPERATOR
    (?‘ . " backquote  ")               ; LEFT SINGLE QUOTATION MARK
    (?’ . "'")                          ; right SINGLE QUOTATION MARK
    (?‐ . "-")                          ; hyphen
    (?– . "--")                         ; n-dash
    (?— . "---")                        ; m-dash
    (?― . "----")                       ; horizontal bar 
    (?‖ . "||")                         ; vertical bar
    (?… . "...")                        ; ellipses
    (?• . " bullet ")                   ; bullet
    (? . " ... ")                   ; message-waiting
    (?™ . "TM")                         ; trademark
    (?ﬀ . "ff")                         ; latin small ligature ff
    (?ﬁ . "fi")                         ; latin small ligature fi
    (?ﬂ . "fl")                         ; latin small ligature fl
    (?ﬃ . "ffi")                        ; latin small ligature ffi
    (?ﬄ . "Ffl")                        ; latin small ligature ffl
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
;;{{{ Variables

(defvar dtk-unicode-untouched-charsets
  '(ascii latin-iso8859-1)
  "*Characters of these charsets are completely ignored by dtk-unicode-replace-chars.")

(defvar dtk-unicode-handlers
  '(dtk-unicode-user-table-handler dtk-unicode-full-table-handler)
  "List of functions which are called in in this order for replacing an unspeakable character.

A handler returns a non-nil value if the   replacement was successful, nil otherwise.")

;;}}}
;;{{{ Helper functions

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
          
(defun dtk-unicode-build-skip-regexp (charsets)
  "Construct regexp to match all but the characters in dtk-unicode-untouched-charsets."
  (format "[^%s]"
          (loop for charset in charsets
                when (charsetp charset)
                concat (apply 'format "%c-%c" (dtk-unicode-charset-limits charset)))))

(defvar dtk-unicode-charset-filter-regexp
  (dtk-unicode-build-skip-regexp dtk-unicode-untouched-charsets)
  "Regular exppression that matches characters not in dtk-unicode-untouched-charsets.")

(defun dtk-unicode-update-untouched-charsets (charsets)
  "Update list of charsets we will not touch."
  (setq dtk-unicode-untouched-charsets charsets)
  (setq dtk-unicode-charset-filter-regexp (dtk-unicode-build-skip-regexp dtk-unicode-untouched-charsets)))

(eval-and-compile
  (if (> emacs-major-version 22)
      (progn
        (defmacro with-charset-priority (charsets &rest body)
          "Execute BODY like `progn' with CHARSETS at the front of priority list.
CHARSETS is a list of charsets.  See
`set-charset-priority'.  This affects the implicit sorting of lists of
charsets returned by operations such as `find-charset-region'."
          (let ((current (make-symbol "current")))
            `(let ((,current (charset-priority-list)))
               (apply #'set-charset-priority ,charsets)
               (unwind-protect
                   (progn ,@body)
                 (apply #'set-charset-priority ,current)))))

        (defun dtk-unicode-char-in-charsets-p  (char charsets)
          "Return t if CHAR is a member of one in the charsets in CHARSETS."
          (with-charset-priority charsets
                                 (memq (char-charset char) charsets))))
    ;; emacs-major-version <= 22
    (defun dtk-unicode-char-in-charsets-p (char charsets)
      "Return t if CHAR is a member of one in the charsets in CHARSETS."
      (memq (char-charset char) charsets))))

(defsubst dtk-unicode-char-untouched-p (char)
  "Return t if char is a member of one of the charsets in dtk-unicode-untouched-charsets."
  (dtk-unicode-char-in-charsets-p char dtk-unicode-untouched-charsets))

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

(defsubst dtk-unicode-char-properties (char)
  "Return unicode properties for CHAR.

Converts char to unicode if necessary (for emacs 22)."
  (let ((unicode (encode-char char 'ucs)))
    (and unicode (condition-case nil
                     (let ((emacspeak-speak-cue-errors nil)
                           (emacspeak-speak-messages nil))
                       (describe-char-unicode-data unicode))
                   (error nil)))))

(defsubst dtk-unicode-char-property (char prop-name)
  "Get character property by name."
  (second (assoc prop-name (dtk-unicode-char-properties char))))

(defun dtk-unicode-name-for-char (char)
  "Return unicode name for character CHAR.

nil if CHAR is not in Unicode."
  (let ((name (dtk-unicode-char-property char "Name")))
    (when (and (stringp name) (string-equal name "<control>"))
      (setq name (dtk-unicode-char-property char "Old name")))
    (and (stringp name) (downcase name))))

(defsubst dtk-unicode-char-punctuation-p (char)
  "Use unicode properties to determine whether CHAR is a ppunctuation character."
  (let ((category (dtk-unicode-char-property char "Category"))
        (case-fold-search t))
    (when (stringp category)
      (string-match "punctuation" category))))

(defsubst dtk-unicode-apply-name-transformation-rules (name)
  "Apply transformation rules in dtk-unicode-name-transformation-rules-alist to NAME."
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
;;{{{ Character replacement handlers

(defsubst dtk-unicode-user-table-handler (char)
  "Return user defined replacement character if it exists."
  (cdr (assq char dtk-unicode-character-replacement-alist)))
        

(defsubst dtk-unicode-full-table-handler (char)
  "Uses the unicode data file to find the name of CHAR."
  (let ((char-desc (dtk-unicode-name-for-char char)))
    (when char-desc
      (format  " %s " (dtk-unicode-apply-name-transformation-rules char-desc)))))

;;}}}
;;{{{ External interface

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
               (replacement
                (save-match-data  
                  (if (and (eq mode 'none) (dtk-unicode-char-punctuation-p char))
                      " "
                    (run-hook-with-args-until-success 'dtk-unicode-handlers char)))))
          (when replacement
            (let ((props (text-properties-at pos)))
              (replace-match replacement t t nil)
              (when props
                (set-text-properties pos (point) props)))))))))

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
