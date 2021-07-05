;;; dtk-unicode.el --- Pronounce Unicode characters -*- lexical-binding: t; -*-
;;{{{ Header: Lukas

;; Copyright 2007, 2011 Lukas Loehrer
;;; TVR: Integrated into Emacspeak July 6, 2008
;;; Using patch from Lukas.
;;
;; Author: Lukas Loehrer <loehrerl |at| gmx.net>
;; Version: $Id$
;; Keywords:  TTS, Unicode

;;}}}
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2008-07-06 10:18:30 -0700 (Sun, 06 Jul 2008) $ |
;;;  $Revision: 4670 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ Introduction

;;; Commentary:

;;; This  Provides Unicode support to the speech layer.

;;; Code:

;;}}}
;;{{{ Required Modules:

(require 'cl-lib)
(cl-declaim (optimize (safety 0) (speed 3)))
;;}}}
;;{{{ Customizations

(defcustom dtk-unicode-character-replacement-alist
  '(
    (? . "-")                       ; START OF GUARDED AREA
    (?━ . "-")                          ; horiz bars
    (?┃ . "|")                          ; vertical block
    (?° . " degrees ")                  ; degree sign
    (?℃ . "Degree C")                   ; celsius
    (?℉ . "Degree F ")                  ; Fahrenheit
    (?“ . "\"")                         ;LEFT DOUBLE QUOTATION MARK
    (?” . "\"")                         ; RIGHT DOUBLE QUOTATION MARK
    (?⋆ . "*")                          ; STAR OPERATOR
    (?­ . "-")                          ; soft-hyphen
    (?‘ . "`")                          ; LEFT SINGLE QUOTATION MARK
    (?’ . "'")                          ; right SINGLE QUOTATION MARK
    (?‐ . "-")                          ; hyphenm
    (?– . " -- ")                       ; n-dash
    (?— . " --- ")                      ; m-dash
    (?  . " ")                          ; hair space
    (?﻿ . " ")                           ; zero-width  no-break space
    (?‌ . "") ; zero width non-joiner
    (?​ . " ")                           ; zero-width space
    (?  . " ")                          ; thin space
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
  "Replacements for  characters."
  :group 'dtk
  :type '(alist
          :key-type (character :tag "character")
          :value-type (string :tag "replacement")))

(defcustom dtk-unicode-process-utf8 t
  "Turn this off when working with TTS  engines that handle UTF8. "
  :type 'boolean
  :group 'dtk)

(defcustom dtk-unicode-name-transformation-rules-alist
  '(
    ("BOX DRAWING" . (lambda (s) "."))
    ("^greek\\( small\\| capital\\)? letter \\(.*\\)$"
     . (lambda (s) (match-string 2 s)))
    ("^latin\\( small\\| capital\\)? letter \\(.*\\)$" . (lambda (s)
                                                           (match-string 2 s)))
    ("^DEVANAGARI \\(sign\\|vowel sign\\|letter\\)? \\(.*\\)$"
     . (lambda (s) (match-string 2 s)))
                                        
    )
  "Alist of character name transformation rules."
  :group 'dtk
  :type
  '(repeat
    (cons :value ("." . identity)
          (regexp :tag "pattern")
          (function :tag "transformation"))))

;;}}}
;;{{{ Variables

(defcustom dtk-unicode-untouched-charsets
  '(ascii latin-iso8859-1)
"Characters of these charsets are  ignored by
  dtk-unicode-replace-chars."
  :group 'dtk
  :type '(repeat symbol))

(defvar dtk-unicode-handlers
  '(dtk-unicode-user-table-handler dtk-unicode-full-table-handler)
  "List of functions which are called in this order for replacing
an unspeakable character.  A handler returns a non-nil value if
the replacement was successful, nil otherwise.")

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
    (let* ((chars (charset-chars charset))
           min max)
      (if (eq chars 96)
          (setq min 32 max 127)
        (setq min 33 max 126))
      (list (make-char charset min min) (make-char charset max max))))))

(defun dtk-unicode-build-skip-regexp (charsets)
  "Construct regexp to match all but the characters in
dtk-unicode-untouched-charsets."
  (format "[^%s]"
          (cl-loop for charset in charsets
                   when (charsetp charset)
                   concat
                   (apply
                    #'format
                    "%c-%c" (dtk-unicode-charset-limits charset)))))

(defvar dtk-unicode-charset-filter-regexp
  (dtk-unicode-build-skip-regexp dtk-unicode-untouched-charsets)
  "Regular exppression that matches characters not in
  dtk-unicode-untouched-charsets.")

(defun dtk-unicode-update-untouched-charsets (charsets)
  "Update list of charsets we will not touch."
  (setq dtk-unicode-untouched-charsets charsets)
  (setq dtk-unicode-charset-filter-regexp
        (dtk-unicode-build-skip-regexp dtk-unicode-untouched-charsets)))

(eval-and-compile
  (defmacro dtk--with-charset-priority (charsets &rest body)
    "Execute BODY like `progn' with CHARSETS at the front of priority list.
CHARSETS is a list of charsets.  See
`set-charset-priority'.  This affects the implicit sorting of lists of
charsets returned by operations such as `find-charset-region'."
    (declare (indent 1) (debug t))
    (let ((current (make-symbol "current")))
      `(let ((,current (charset-priority-list)))
         (apply #'set-charset-priority ,charsets)
         (unwind-protect
             (progn ,@body)
           (apply #'set-charset-priority ,current)))))
;;; Now use it:
  (defun dtk-unicode-char-in-charsets-p (char charsets)
    "Return t if CHAR is a member of one in the charsets in CHARSETS."
    (dtk--with-charset-priority charsets (memq (char-charset char) charsets))))

(defun dtk-unicode-char-untouched-p (char)
  "Return t if char is a member of one of the charsets in
dtk-unicode-untouched-charsets."
  (dtk-unicode-char-in-charsets-p char dtk-unicode-untouched-charsets))

(defvar dtk-unicode-cache (make-hash-table)
  "Cache for unicode data lookups.")

(defadvice describe-char-unicode-data (around emacspeak pre act comp)
  "Cache result."
  (let* ((char (ad-get-arg 0))
         (result (gethash char dtk-unicode-cache 'not-found)))
    (if (eq result 'not-found)
        (progn
          ad-do-it
          (puthash char ad-return-value dtk-unicode-cache))
      (setq ad-return-value result))))

(defun dtk-unicode-name-for-char (char)
  "Return unicode name for character CHAR. "
  (cond
   ((= char 128) "")
   (t
    (downcase
     (or
      (get-char-code-property char 'name)
      (get-char-code-property char 'old-name)
      (format "%c" char))))))

(defun dtk-unicode-char-punctuation-p (char)
  "Use unicode properties to determine whether CHAR is a
ppunctuation character."
  (let ((category (get-char-code-property char 'category))
        (case-fold-search t))
    (when (stringp category)
      (string-match "punctuation" category))))

(defun dtk-unicode-apply-name-transformation-rules (name)
  "Apply transformation rules in
dtk-unicode-name-transformation-rules-alist to NAME."
  (funcall
   (or
    (assoc-default
     name
     dtk-unicode-name-transformation-rules-alist 'string-match)
       'identity)
   name))

(defun dtk-unicode-uncustomize-char (char)
  "Delete custom replacement for CHAR.

When called interactively, CHAR defaults to the character after point."
  (interactive (list (following-char)))
  (setq dtk-unicode-character-replacement-alist
        (cl-loop for elem in dtk-unicode-character-replacement-alist
                 unless (eq (car elem) char) collect elem)))

(defun dtk-unicode-customize-char (char replacement)
  "Add a custom replacement string for CHAR.

When called interactively, CHAR defaults to the character after point."
  (interactive
   (let ((char (following-char)))
     (list char
           (read-string
            (format
             "Replacement for %c (0x%x) from charset %s: "
             char char (char-charset char))))))
  (push (cons char replacement) dtk-unicode-character-replacement-alist))

;;}}}
;;{{{ Character replacement handlers

(defun dtk-unicode-user-table-handler (char)
  "Return user defined replacement character if it exists."
  (cdr (assq char dtk-unicode-character-replacement-alist)))

(defun dtk-unicode-full-table-handler (char)
  "Uses the unicode data file to find the name of CHAR."
  (let ((char-desc (dtk-unicode-name-for-char char)))
    (when char-desc
      (format
       " %s " (dtk-unicode-apply-name-transformation-rules char-desc)))))

;;}}}
;;{{{ External interface

(defun dtk-unicode-full-name-for-char (char)
  "Return full name of CHAR. "
  (dtk-unicode-name-for-char char))
(defun dtk-unicode-short-name-for-char (char)
  "Return short name of CHAR. "
  (if (memq char dtk-unicode-untouched-charsets)
      (char-to-string char)
    (dtk-unicode-name-for-char char)))

(defun dtk-unicode-replace-chars (mode)
  "Replace unicode characters in current buffer with something  TTS friendly. "
  (cl-declare (special dtk-unicode-process-utf8))
  (when dtk-unicode-process-utf8
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward dtk-unicode-charset-filter-regexp nil t)
          (let* ((pos (match-beginning 0))
                 (char (char-after pos))
                 (props (text-properties-at pos))
                 (replacement
                  (save-match-data
                    (if (and
                         (memq mode '(some none))
                         (dtk-unicode-char-punctuation-p char))
                        " "
                      (run-hook-with-args-until-success
                       'dtk-unicode-handlers char)))))
            (replace-match replacement t t nil)
            (when props
              (set-text-properties pos (point) props))))))))

;;}}}
(provide 'dtk-unicode)

;;{{{  emacs local variables

;;; local variables:
;;; coding: utf-8
;;; folded-file: t
;;; end:

;;}}}

;;; dtk-unicode.el ends here
