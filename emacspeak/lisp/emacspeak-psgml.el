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

;;;Copyright (C) 1995, 1996, 1997, 1998, 1999   T. V. Raman  
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

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'emacspeak-fix-interactive)
(require 'voice-lock)
;;{{{  Introduction

;;; Commentary:

;;; Speech-enable psgml --a powerful SGML support package.
;;; psgml can be found at 
;;;

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
(defadvice sgml-forward-element (after emacspeak pre act
                                       comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))
(defadvice sgml-backward-element (after emacspeak pre act
                                        comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice sgml-down-element (after emacspeak pre act
                                    comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice sgml-backward-up-element (after emacspeak pre act
                                           comp)
  "Speak line we moved to"
  (when (interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

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


;;}}}
;;{{{ define pronunciations 
(emacspeak-pronounce-add-dictionary-entry 'sgml-mode"CDATA"
                                          "C DATA")
;;; xml mode inherits from sgml mode
(emacspeak-pronounce-add-super 'sgml-mode 'xml-mode)
;;}}}
;;{{{ setup sgml-mode-hook
(add-hook 'sgml-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations "all")
                      (or dtk-split-caps
                          (dtk-toggle-split-caps)))))
                                    
           
             
;;}}}
;;{{{ simple voice locking 


(voice-lock-set-major-mode-keywords 'xml-mode
                                                      'xml-voice-lock-keywords)

(defvar xml-voice-lock-keywords nil
  "Voice lock keywords for XML mode.")

(setq xml-voice-lock-keywords
'(("^<!--.*$"  . voice-lock-comment-personality)
  ("</?[a-zA-Z0-9]+>" . voice-lock-type-personality)
  ("\"[^\"]+\""  . voice-lock-string-personality)))



;;}}}
(provide  'emacspeak-psgml)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
