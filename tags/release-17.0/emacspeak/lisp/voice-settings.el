;;; voice-settings.el --- Defines voice settings used to voice lock different modes
;;; $Id$
;;; $Author$ 
;;; Description:  Voice lock mode for Emacspeak
;;{{{  LCD Archive entry: 
;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile (load-library "cl-extra"))
;;{{{ flexible way of adding new voice lock modes:

(defvar voice-lock-mode-specific-keywords-table
  (make-hash-table  )
  "Association between major mode names and the voice lock keywords to
be used in that mode.")

(defun voice-lock-set-major-mode-keywords (mode keywords)
  "Set voice lock keywords for major mode."
  (declare (special
            voice-lock-mode-specific-keywords-table))
  (setf (gethash mode voice-lock-mode-specific-keywords-table )
        keywords))

(defun voice-lock-get-major-mode-keywords (mode)
  "Retrieve variable holding voice lock keywords for this mode.
Return nil if not set."
  (declare (special voice-lock-mode-specific-keywords-table))
  (or  (gethash mode voice-lock-mode-specific-keywords-table)
       nil))

;;}}}
;;{{{  additional voice lock keywords for new modes

(defvar texi-voice-lock-keywords
  (list
   "@\\(@\\|[^}\t \n{]+\\)"             ;commands
   '("^\\(@c\\|@comment\\)[ \t].*$" . voice-lock-comment-personality) ;comments
   '("^\\(*.*\\)[\t ]*$" 1 voice-lock-function-name-personality t) ;menu items
   '("@\\(emph\\|strong\\|b\\|i\\){\\([^}]+\\)" 2 voice-lock-comment-personality t)
   '("@\\(file\\|kbd\\|key\\){\\([^}]+\\)" 2 voice-lock-string-personality t)
   '("@\\(samp\\|code\\|var\\){\\([^}]+\\)" 2 voice-lock-function-name-personality t)
   '("@\\(xref\\|pxref\\){\\([^}]+\\)" 2 voice-lock-keyword-personality t)
   '("@end *\\([a-zA-Z0-9]+\\)[ \t]*$" 1 voice-lock-function-name-personality t)
   '("@item \\(.*\\)$" 1 voice-lock-function-name-personality t)
   '("\\$\\([^$]*\\)\\$" 1 voice-lock-string-personality t)
   )
  "Additional expressions to highlight in TeXinfo mode.")

(defvar dired-voice-lock-keywords
  '( ;; Put directory headers in italics.
    ("^  \\(/.+\\)$" 1 voice-lock-type-personality)
    ;; Put symlinks in bold italics.
    ("\\([^ ]+\\) -> [^ ]+$" . voice-lock-function-name-personality)
    ;; Put marks (entire line) in bold.
    ("^\\([^ ]\\).*$" .  voice-lock-comment-personality)
    ;; Put files that are subdirectories in bold.
    ("^..d.* \\([^ ]+\\)$" . voice-lock-keyword-personality))
  "Additional expressions to highlight in Dired mode.")

(defvar rmail-voice-lock-keywords
  '( ;; Put From field in bold.
    ("^From: \\(.*\\)$" 1 voice-lock-keyword-personality)
    ;; Put subject in bold italics
    ("^Subject: \\(.*\\)$" 1 voice-lock-function-name-personality))
  "Additional expressions to highlight in Rmail mode.")

(defvar rmail-summary-voice-lock-keywords
  '(("^\\s *[0-9]+D.*$" . voice-lock-doc-string-personality)
    ("^\\s *[0-9]+-.*$" . voice-lock-keyword-personality))
  "Additional expressions to highlight in Rmail Summary mode.")

(defvar compilation-mode-voice-lock-keywords
  '(("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 1 voice-lock-function-name-personality))
  "Additional expressions to highlight in Compilation mode.")

(defvar help-voice-lock-keywords nil
  "Voice lock keywords for help mode.")
(setq help-voice-lock-keywords
      '(("\\`\\([-+a-zA-Z0-9_*]+\\)\\(\\(:\\)\\|\\('\\)\\)" (1 (if
                                                                   (match-beginning 3) voice-lock-function-name-personality voice-lock-variable-name-personality))) ("`\\([-+a-zA-Z0-9_:*][-+a-zA-Z0-9_:*]+\\)'" 1 voice-lock-reference-personality t) ("\\<:[-+a-zA-Z0-9_:*]+\\>" 0 voice-lock-reference-personality t)))

;;{{{ scheme mode 
(defvar emacspeak-scheme-voice-lock-keywords 
  '(t
    ("(\\(define\\*?\\(\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\)\\|\\(-syntax\\)\\|-class\\|-module\\)\\)\\>[ 	]*(?\\(\\sw+\\)?"
     (1 voice-lock-keyword-personality)
     (6
      (cond
       ((match-beginning 3)
        voice-lock-function-name-personality)
       ((match-beginning 5)
        voice-lock-variable-name-personality)
       (t voice-lock-type-personality))
      nil t))
    ("(\\(and\\|begin\\|c\\(a\\(ll\\(-with-\\(current-continuation\\|input-file\\|output-file\\)\\|/cc\\)\\|se\\)\\|ond\\)\\|d\\(elay\\|o\\)\\|else\\|for-each\\|if\\|l\\(ambda\\|et\\(\\*\\|-syntax\\|rec\\(-syntax\\)?\\)?\\)\\|map\\|or\\|syntax\\(-rules\\)?\\)\\>"
     (1 voice-lock-keyword-personality))
    ("\\<<\\sw+>\\>"
     (0 voice-lock-type-personality))
    ("\\<:\\sw+\\>"
     (0 voice-lock-builtin-personality)))
  "keywords for voice locking scheme code.")

;;}}}
;;}}}
;;{{{ set voice lock keywords for various modes

;;; Note: later these may move to their appropriate extension modules.
(voice-lock-set-major-mode-keywords 'scheme-mode
                                    'emacspeak-scheme-voice-lock-keywords)
(voice-lock-set-major-mode-keywords 'help-mode
                                    'help-voice-lock-keywords)
(voice-lock-set-major-mode-keywords 'texinfo-mode
                                    'texi-voice-lock-keywords)
(voice-lock-set-major-mode-keywords 'shell-mode
                                    'shell-voice-lock-keywords)
(voice-lock-set-major-mode-keywords 'dired-mode
                                    'dired-voice-lock-keywords)
(voice-lock-set-major-mode-keywords 'rmail-mode
                                    'rmail-voice-lock-keywords)

(voice-lock-set-major-mode-keywords 'rmail-summary-mode
                                    'rmail-summary-voice-lock-keywords)
(voice-lock-set-major-mode-keywords 'compilation-mode
                                    'compilation-mode-voice-lock-keywords)

;;}}}
;;{{{  use gaudy settings

(declaim (special voice-lock-maximum-decoration))
(setq voice-lock-maximum-decoration t)

;;}}}
;;{{{ autoload fast voice lock 
(autoload 'fast-voice-lock-mode
  "fast-voice-lock"
  "fast lock support for voice locking ")
;;}}}
;;{{{ turn on lazy lock
(autoload 'lazy-voice-lock-mode
  "lazy-voice-lock"
  "lazy lock support for voice locking ")
(declaim (special voice-lock-support-mode))

(setq voice-lock-support-mode
      '(
        (xae-mode . lazy-voice-lock-mode)
        (xml-mode . lazy-voice-lock-mode)
        (sgml-mode . lazy-voice-lock-mode)
        (c-mode . lazy-voice-lock-mode)
        (c++-mode . lazy-voice-lock-mode)
        (java-mode . lazy-voice-lock-mode)
        (objc-mode . lazy-voice-lock-mode)
        (lisp-mode . lazy-voice-lock-mode)
        (emacs-lisp-mode . lazy-voice-lock-mode)
        (lisp-interaction-mode . lazy-voice-lock-mode)
        (perl-mode . lazy-voice-lock-mode)
        (tex-mode . lazy-voice-lock-mode)
        (tcl-mode . lazy-voice-lock-mode)
        (tex-mode . lazy-voice-lock-mode)
        (latex-mode . lazy-voice-lock-mode)
        (latex2e-mode . lazy-voice-lock-mode)
        (vm-mode . lazy-voice-lock-mode)
        (dired-mode . lazy-voice-lock-mode)))
;;}}}
(provide 'voice-settings)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
