;;; emacspeak-xslide.el --- Speech enable  XSL authoring 
;;; $Id$
;;; $Author$
;;; Description:   extension to speech enable xslide 
;;; Keywords: Emacspeak, Audio Desktop
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

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{  Introduction:

;;; Commentary:
;;; xslide is an emacs package for authoring and maintaining
;;; XSL stylesheets
;;; xslide is at http://www.mulberrytech.com/xsl/xslide/index.html
;;; this module speech-enables xslide

;;; Code:

;;}}}
;;{{{  speech-enable interactive commands

(defadvice xsl-electric-apos (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice xsl-electric-quote (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))
(defadvice xsl-electric-lsqb (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))
(defadvice xsl-electric-lpar (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice xsl-electric-lcub (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))
(defadvice xsl-electric-less-than (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice xsl-electric-slash (after emacspeak pre act comp)
  "Speak char we inserted."
  (when (interactive-p)
    (emacspeak-speak-this-char (preceding-char))))

(defadvice xsl-complete (around emacspeak pre act com)
  "Say what you completed"
  (let ((prior (point ))
        (emacspeak-speak-messages nil))
    (emacspeak-kill-buffer-carefully "*Completions*")
    ad-do-it
    (let ((completions-buffer (get-buffer " *Completions*")))
      (if (> (point) prior)
          (dtk-speak (buffer-substring prior (point )))
        (when (and completions-buffer
                   (window-live-p (get-buffer-window completions-buffer )))
          (save-excursion
            (set-buffer completions-buffer )
            (emacspeak-prepare-completions-buffer)
            (dtk-speak (buffer-string ))))))
    ad-return-value))
(defadvice xsl-mode (after emacspeak pre act comp)
  "set up for voice locking."
  (emacspeak-xsl-voice-lock-setup)
  (voice-lock-mode 1)
  (dtk-set-punctuations "all"))

(defun emacspeak-xsl-voice-lock-setup()
  "Setup voice locking for xsl mode."
  (declare (special voice-lock-defaults))
  (make-local-variable 'voice-lock-defaults)
  (setq voice-lock-defaults '(xsl-voice-lock-keywords t)))

;;}}}
;;{{{ voice locking 

(defvar xsl-xsl-alternate-personality
  (tts-get-voice-command 'paul-animated)
  "Personality used in xsl highlighting.")
(defvar xsl-fo-alternate-personality 'paul-monotone 
  "Personality used in XSL highlighting.")

(defvar xsl-other-element-personality 'paul-surprized
  "Personality used in XSL highlighting.")

(defvar xsl-xsl-main-personality 'harry 
  "Personality used for highlighting in XSL.")
;;; keep byte compiler happy 

(declaim (special 
          xsl-fo-attribute-symbol-alist-2
          xsl-fo-attribute-symbol-alist-1
          xsl-fo-symbol-alist
          xsl-fo-ns-prefix
          xsl-attributes-alist
          xsl-element-symbol-alist
          xsl-xsl-ns-prefix))

(defvar xsl-voice-lock-keywords
  (list
   ;;
   ;; Reserved XML Processing instruction
   ;;
   '(
     "\\(<\\?\\)\\(xml\\)\\(\\s-+version\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+encoding\\s-*=\\s-*\\('[^']+'\\|\"[^\"]+\"\\)\\)?\\(\\s-+standalone\\s-*=\\s-*\\('\\(yes\\|no\\)'\\|\"\\(yes\\|no\\)\"\\)\\)?\\s-*\\(\\?>\\)"
     (1 voice-lock-keyword-personality)
     (2 voice-lock-type-personality nil)
     (3 voice-lock-type-personality nil t)
     (5 voice-lock-type-personality nil t)
     (7 voice-lock-type-personality nil t)
     (11 voice-lock-keyword-personality))
   ;;
   ;; Non-reserved XML Processing instruction
   ;; Any XML PI that doesn't start with "<?xml"
   ;;
   '("\\(<\\?\\)\\([^ \t?>]+\\)[ \t]*\\([^?>]\\|\\?[^>]\\|>[^\n\r]\\)*\\(\\?>\\)"
     (1 voice-lock-keyword-personality)
     (2 voice-lock-variable-name-personality)
     (4 voice-lock-keyword-personality))
   ;;
   ;; Entity references
   ;; These come early so entity references as the names in element, etc.
   ;; declarations retain their colour and don't get turned into
   ;; voice-lock-variable-name-personality.  E.g:
   ;; <!ENTITY % %entity; "..." >
   ;;
   '("[%&][^; \t]+;" . voice-lock-reference-personality)
   ;;
   ;; Marked section start
   ;;
   '("\\(<!\\[\\)[^[]*\\(\\[\\)"
     (1 voice-lock-keyword-personality)
     (2 voice-lock-keyword-personality))
   ;;
   ;; Text inside <xsl:text>
   (list
    (concat "<" xsl-xsl-ns-prefix ":text>"
	    "\\([^<]*\\)"
	    "</" xsl-xsl-ns-prefix ":text>")
    '(1 voice-lock-string-personality append))
   ;;
   ;; "Other" tags
   ;;
   (list
    (concat "\\(</?\\([^xf/\?!]\\|x[^s]\\|xs[^l]\\|xsl[^:]\\|f[^o]\\|fo[^:]\\)\\([^</>]\\|/[^>]\\)*/?>\\)")
    '(1 xsl-other-element-personality t))
   ;;
   ;; Content of tags
   ;;
   (list
    (concat ">\\([^<]+\\)<")
    '(1 voice-lock-string-personality keep))
   ;;
   ;; XSL elements
   ;;
   (list
    (concat "\\(</?\\)\\(" xsl-xsl-ns-prefix ":\\)\\("
	    ;;	    (make-regexp xsl-elements)
	    (make-regexp
	     (mapcar 'car xsl-element-symbol-alist))
	    "\\)\\(\\s-+\\([^/>]\\|/[^>]\\)+\\)*\\(/?>\\)")
    '(1 xsl-xsl-main-personality)
    '(2 xsl-xsl-alternate-personality)
    '(3 xsl-xsl-main-personality))
   (list
    (concat "</?" xsl-xsl-ns-prefix ":\\([^/>]\\|/[^>]\\)*\\(/?>\\)")
    '(2 xsl-xsl-main-personality))
   ;;
   ;; XSL attributes
   ;;
   ;;   (list
   ;;    (concat "\\b\\("
   ;;	    (make-regexp xsl-attributes)
   ;;	    "\\)[ \t]*=\"[^\"]*\"")
   ;;    '(1 voice-lock-keyword-personality))
   (make-regexps "\\b"
		 (list (mapcar 'car xsl-attributes-alist)
		       xsl-xsl-alternate-personality)
		 "[ \t]*"
		 '(("=[ \t]*\"") xsl-xsl-alternate-personality)
		 '("\\([^\"<>]*\\)" 1 voice-lock-variable-name-personality)
		 '(("\"") xsl-xsl-alternate-personality))
   (make-regexps "\\b"
		 (list (mapcar 'car xsl-attributes-alist)
		       xsl-xsl-alternate-personality)
		 "[ \t]*"
		 '(("=[ \t]*'") xsl-xsl-alternate-personality)
		 '("\\([^'<>]*\\)" 1 voice-lock-variable-name-personality)
		 '(("'") xsl-xsl-alternate-personality))
   ;;
   ;; XSL formatting objects
   ;;
   (list
    (concat "\\(</?\\)\\(" xsl-fo-ns-prefix ":\\)\\("
	    (make-regexp
	     (mapcar 'car xsl-fo-symbol-alist))
	    "\\)\\(\\s-+\\([^/>]\\|/[^>]\\)+\\)*\\(/?>\\)")
    '(1 xsl-fo-main-personality)
    '(2 xsl-fo-alternate-personality)
    '(3 xsl-fo-main-personality))
   (list
    (concat "</?" xsl-fo-ns-prefix ":\\([^/>]\\|/[^>]\\)*\\(/?>\\)")
    '(2 xsl-fo-main-personality))
   ;;
   ;; XSL formatting object properties
   ;;
   (make-regexps "\\b"
		 (list (mapcar 'car xsl-fo-attribute-symbol-alist-1)
		       xsl-fo-alternate-personality)
		 "[ \t]*"
		 '(("=[ \t]*\"") xsl-fo-alternate-personality)
		 '("\\([^\"<>]*\\)" 1 voice-lock-variable-name-personality)
		 '(("\"") xsl-fo-alternate-personality))
   (make-regexps "\\b"
		 (list (mapcar 'car xsl-fo-attribute-symbol-alist-1)
		       xsl-fo-alternate-personality)
		 "[ \t]*"
		 '(("=[ \t]*'") xsl-fo-alternate-personality)
		 '("\\([^'<>]*\\)" 1 voice-lock-variable-name-personality)
		 '(("'") xsl-fo-alternate-personality))
   (make-regexps "\\b"
		 (list (mapcar 'car xsl-fo-attribute-symbol-alist-2)
		       xsl-fo-alternate-personality)
		 "[ \t]*"
		 '(("=[ \t]*\"") xsl-fo-alternate-personality)
		 '("\\([^\"<>]*\\)" 1 voice-lock-variable-name-personality)
		 '(("\"") xsl-fo-alternate-personality))
   (make-regexps "\\b"
		 (list (mapcar 'car xsl-fo-attribute-symbol-alist-2)
		       xsl-fo-alternate-personality)
		 "[ \t]*"
		 '(("=[ \t]*'") xsl-fo-alternate-personality)
		 '("\\([^'<>]*\\)" 1 voice-lock-variable-name-personality)
		 '(("'") xsl-fo-alternate-personality))
   ;;
   ;; Mark the start and end of literals, but don't do anything to their
   ;; contents
   ;;
   '("\\('\\)[^']*\\('\\)"
     (1 voice-lock-string-personality)
     (2 voice-lock-string-personality))
   '("\\(\"\\)[^\"]*\\(\"\\)"
     (1 voice-lock-string-personality)
     (2 voice-lock-string-personality))
   ;;
   ;; { } in attribute values
   ;;
   '("\\('\\|\"\\)\\([^{\\1]\\|{{\\)*\\({[^\\1}]*}\\)\\([^{\\1]\\|{{\\)*\\(\\1\\)"
     (3 voice-lock-variable-name-personality t))
   ;;
   ;; Put comment patterns last so they mask anything
   ;; that might be inside the comment
   ;;
   '("\\(<!--[^-]*\\(-[^-]+\\)*-->\\)"
     (1 voice-lock-comment-personality t))
   )
  "Additional expressions to highlight in XSL mode.")

;;}}}
(provide 'emacspeak-xslide)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
