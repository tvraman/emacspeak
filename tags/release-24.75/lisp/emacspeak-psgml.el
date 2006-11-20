;;; emacspeak-sgml-mode.el --- Speech enable psgml package
;;; $Id$
;;; $Author$ 
;;; Description: Emacspeak extension for psgml
;;; Keywords:emacspeak, audio interface to emacs psgml
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

;;;Copyright (C) 1995 -- 2006, T. V. Raman 
;;; Copyright (c) 1995 by T. V. Raman  
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

;;{{{  Introduction

;;; Commentary:

;;; Speech-enable psgml --a powerful SGML support package.
;;; psgml can be found at 

;;; Code:
;;}}}
;;{{{ requires
(require 'emacspeak-preamble)

;;}}}
;;{{{  helpers 

(defsubst emacspeak-psgml-summarize-element ()
  "Context-sensitive element summarizer."
  (interactive)
  (cond
   ((eq major-mode 'emacspeak-xml-browse-mode)
    (emacspeak-psgml-speak-current-element))
   (t (emacspeak-speak-line))))

;;}}}
;;{{{ advice interactive commands 

(defadvice sgml-close-angle (around emacspeak pre act comp)
  "Speak what we matched"
  (cond
   ((interactive-p)
    (emacspeak-speak-this-char ?>)
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice sgml-slash (around emacspeak pre act comp)
  "Speak what we inseerted"
  (cond
   ((interactive-p)
    (emacspeak-speak-this-char ?/)
    ad-do-it)
   (t ad-do-it))
  ad-return-value)

(defadvice sgml-list-valid-tags (after emacspeak pre act
                                       comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'help)
    (message "Valid tags displayed in TAGS buffer in other
window")))

(defadvice sgml-complete (around emacspeak pre act com)
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

(defadvice sgml-insert-end-tag (after emacspeak pre act
                                      comp)
  "Say what you inserted"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)

    (emacspeak-speak-line)))
(defadvice sgml-forward-element (after emacspeak pre act comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-psgml-summarize-element)))

(defadvice sgml-backward-element (after emacspeak pre act
                                        comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-psgml-summarize-element)))

(defadvice sgml-down-element (after emacspeak pre act
                                    comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-psgml-summarize-element)))

(defadvice sgml-backward-up-element (after emacspeak pre act comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-psgml-summarize-element)))

(defadvice sgml-beginning-of-element (after emacspeak pre act
                                            comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line))))

(defadvice sgml-end-of-element (after emacspeak pre act
                                      comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line))))
(defadvice sgml-kill-element (after emacspeak pre act comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'delete-object)
      (emacspeak-speak-line))))

(defadvice sgml-up-element (after emacspeak pre act
                                  comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line))))

(defadvice sgml-next-data-field (after emacspeak pre act
                                       comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line))))

(defadvice sgml-next-trouble-spot (after emacspeak pre act
                                         comp)
  "Speak line we moved to"
  (when (interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-line))))
(defadvice sgml-mark-element (after emacspeak pre act comp)
  "Say what we did"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked  element")))

(defadvice sgml-mark-current-element (after emacspeak pre act comp)
  "Say what we did"
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (message "Marked current element")))

(defadvice sgml-fold-element (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice sgml-fold-subelement (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice sgml-fold-region (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice sgml-expand-element (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice sgml-unfold-all (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice sgml-unfold-element (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice sgml-unfold-line (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice sgml-split-element (after emacspeak pre act comp)
  "provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Split current element")))

(defadvice sgml-hide-tags (after emacspeak pre act comp)
  "Announce what you just did."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Hid all markup tags.")))
(defadvice sgml-hide-attributes (after emacspeak pre act comp)
  "Announce what you just did."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Hid all markup attributes.")))

(defadvice sgml-show-tags (after emacspeak pre act comp)
  "Announce what you just did."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Showing  all markup tags.")))

(defadvice sgml-show-attributes (after emacspeak pre act comp)
  "Announce what you just did."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (message "Showing all markup attributes.")))

;;}}}
;;{{{  editting attributes 

(defadvice sgml-edit-attributes (after emacspeak pre act
                                       comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))
(defadvice sgml-edit-attrib-finish(after emacspeak pre act
                                         comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))
(defadvice sgml-edit-attrib-field-start (after emacspeak pre
                                               act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice sgml-edit-attrib-field-end (after emacspeak pre
                                             act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))
(defadvice sgml-edit-attrib-next (after emacspeak pre
                                        act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice sgml-edit-attrib-clear (after emacspeak pre act
                                         comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'delete-object)
    (emacspeak-speak-line)))

(defadvice sgml-edit-attrib-default  (after emacspeak pre act
                                            comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))
(defadvice sgml-edit-attrib-mode (after emacspeak pre act
                                        comp)
  "Fix keymap"
  (define-key sgml-edit-attrib-mode-map "\C-e"
    'emacspeak-prefix-command)
  (local-set-key "\C-e e" 'sgml-edit-attrib-field-end))

;;}}}
;;{{{ define pronunciations 

(emacspeak-pronounce-add-dictionary-entry 'sgml-mode"CDATA"
                                          "C DATA")
(declaim (special emacspeak-pronounce-common-xml-namespace-uri-pronunciations))

;;; xml mode inherits from sgml mode
(emacspeak-pronounce-augment-pronunciations 'xml-mode
                                            emacspeak-pronounce-common-xml-namespace-uri-pronunciations)
(emacspeak-pronounce-add-super 'sgml-mode 'xml-mode)
;;}}}
;;{{{ setup sgml-mode-hook
(declaim (special sgml-mode-map))
(add-hook
 'sgml-mode-hook
 (function
  (lambda ()
    (declare (special sgml-mode-map))
    (emacspeak-setup-programming-mode)
    (define-key sgml-mode-map "\C-c\C-b"
      'emacspeak-xml-browse-mode))))
           
;;}}}
;;{{{ psgml based voice locking 

(defvar emacspeak-sgml-markup-voices
  (list 
   (cons 'start-tag       voice-bolden)
   (cons 'end-tag         voice-bolden)
   (cons 'comment         voice-monotone)
   (cons 'pi       voice-animate)
   (cons 'sgml    voice-animate)
   (cons 'doctype         voice-animate)
   (cons 'entity          voice-animate)
   (cons 'shortref     voice-bolden))
  "*List of markup to personality mappings.
Element are of the form (MARKUP-TYPE . personality).
Possible values for MARKUP-TYPE is:
comment - comment declaration
doctype - doctype declaration
end-tag 
ignored - ignored marked section
ms-end  - marked section start, if not ignored 
ms-start- marked section end, if not ignored
pi      - processing instruction
sgml    - SGML declaration
start-tag
entity  - general entity reference
shortref- short reference")

;;}}}
;;{{{ additional interactive commands 

(defun emacspeak-psgml-speak-current-element ()
  "Speak contents of current element. "
  (interactive)
  (save-excursion
    (sgml-mark-current-element)
    (emacspeak-speak-region (mark) (point))))

;;}}}
;;{{{ sgml browsing mode 

;;; convenience minor mode for browsing sgml and xml
;;; documents.

(define-derived-mode emacspeak-xml-browse-mode xml-mode 
  "Browsing XML documents. "
  "Mode for browsing XML documents.\n\n
\\{emacspeak-xml-browse-mode}")

(declaim (special emacspeak-xml-browse-mode-map ))
(define-key emacspeak-xml-browse-mode-map " "
  'emacspeak-psgml-speak-current-element)
(define-key emacspeak-xml-browse-mode-map [up]
  'sgml-backward-up-element)
(define-key emacspeak-xml-browse-mode-map [down] 'sgml-down-element)
(define-key emacspeak-xml-browse-mode-map [left] 'sgml-backward-element)
(define-key emacspeak-xml-browse-mode-map [right] 'sgml-forward-element)
(define-key emacspeak-xml-browse-mode-map "\C-ch"
  'sgml-hide-tags)
(define-key emacspeak-xml-browse-mode-map "\C-cu" 'sgml-show-tags)
;;}}}
;;{{{  toggle interactive parse:
;;; silence psgml messages
(defadvice sgml-message (around emacspeak pre act comp)
  "Silence messages."
  (let ((emacspeak-speak-messages nil))
    ad-do-it))
(defun emacspeak-psgml-toggle-interactive-font-lock()
  "Toggles variable sgml-set-face.
When turned on, the  buffer is font locked interactively.
Leave this off in general while editting."
  (interactive)
  (declare (special sgml-set-face))
  (setq sgml-set-face (not sgml-set-face))
  (message "Turned %s sgml-set-face "
           (if sgml-set-face "on" "off")))

;;}}}
(provide  'emacspeak-psgml)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
