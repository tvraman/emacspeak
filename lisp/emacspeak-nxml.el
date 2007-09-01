;;; emacspeak-nxml.el --- Speech enable nxml mode
;;; $Id$
;;; $Author$
;;; Description: Controlling mplayer from emacs 
;;; Keywords: Emacspeak, nxml streaming media 
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2007, T. V. Raman
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

;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{ Introduction:

;;; nxml-mode is a new XML mode for emacs by James Clark.
;;;  http://www.thaiopensource.com/download/

;;}}}
;;{{{ customization:

(defgroup emacspeak-nxml nil
  "Customizations for Emacspeak with NXML."
  :group  'emacspeak)

;;}}}
;;{{{ voice locking 
(voice-setup-add-map
 '(
   (nxml-attribute-colon-face voice-monotone)
   (nxml-attribute-local-name-face voice-animate)
   (nxml-attribute-prefix-face voice-monotone-medium)
   (nxml-attribute-value-delimiter-face voice-smoothen)
   (nxml-attribute-value-face voice-lighten)
   (nxml-cdata-section-CDATA-face voice-animate-extra)
   (nxml-cdata-section-content-face  voice-monotone )
   (nxml-cdata-section-delimiter-face voice-monotone-medium)
   (nxml-char-ref-delimiter-face voice-smoothen)
   (nxml-char-ref-number-face voice-animate-medium)
   (nxml-comment-content-face voice-monotone)
   (nxml-comment-delimiter-face  voice-smoothen-medium)
   (nxml-delimited-data-face voice-animate-medium)
   (nxml-delimiter-face voice-bolden-medium)
   (nxml-element-colon-face voice-monotone)
   (nxml-element-local-name-face voice-bolden)
   (nxml-element-prefix-face voice-monotone-medium)
   (nxml-entity-ref-delimiter-face voice-smoothen)
   (nxml-entity-ref-name-face voice-lighten-medium)
   (nxml-hash-face voice-monotone)
   (nxml-markup-declaration-delimiter-face voice-smoothen)
   (nxml-name-face voice-animate-extra)
   (nxml-namespace-attribute-colon-face voice-monotone)
   (nxml-namespace-attribute-prefix-face voice-animate)
   (nxml-namespace-attribute-value-delimiter-face voice-smoothen)
   (nxml-namespace-attribute-value-face voice-lighten)
   (nxml-namespace-attribute-xmlns-face voice-smoothen-extra)
   (nxml-processing-instruction-content-face voice-animate)
   (nxml-processing-instruction-delimiter-face voice-lighten-extra)
   (nxml-processing-instruction-target-face voice-animate-extra)
   (nxml-prolog-keyword-face voice-animate-extra)
   (nxml-prolog-literal-content-face voice-monotone-medium)
   (nxml-prolog-literal-delimiter-face voice-monotone)
   (nxml-ref-face voice-animate-medium)
   (nxml-tag-delimiter-face voice-smoothen)
   (nxml-tag-slash-face voice-smoothen-medium)
   (rng-error-face voice-bolden-and-animate)))

;;}}}
;;{{{ pronunciations 
(declaim (special emacspeak-pronounce-common-xml-namespace-uri-pronunciations))

;;; nxml mode inherits from xml mode
(emacspeak-pronounce-augment-pronunciations 'xml-mode
                                            emacspeak-pronounce-common-xml-namespace-uri-pronunciations)
(emacspeak-pronounce-add-super 'xml-mode 'nxml-mode)

;;}}}
;;{{{ Advice interactive commands

(defadvice nxml-electric-slash (around emacspeak pre act comp)
  "Provide spoken feedback."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))
      (when (= (preceding-char) ?>)
        (emacspeak-auditory-icon 'close-object))))
   (t ad-do-it))
  ad-return-value)
    

(defadvice nxml-complete (around emacspeak pre act comp)
  "Provide spoken feedback."
  (cond
   ((interactive-p)
    (let ((start (point)))
      ad-do-it
      (emacspeak-speak-region start (point))))
   (t ad-do-it))
  ad-return-value)
(defadvice nxml-insert-xml-declaration (after emacspeak pre act
                                              comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-speak-line)))
(loop for f in 
      '(nxml-backward-up-element
        nxml-forward-balanced-item
        nxml-up-element
        nxml-forward-paragraph
        nxml-backward-paragraph
        nxml-backward-single-paragraph
        nxml-backward-single-balanced-item
        nxml-forward-element
        nxml-backward-element)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line)))))

(loop for f in 
      '(nxml-balanced-close-start-tag-block
        nxml-finish-element
        nxml-balanced-close-start-tag-inline)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (dtk-speak
             (format "Closed %s"
                     (xmltok-start-tag-qname)))))))
;;{{{ speech enable outliner 

(loop for f in
      '(nxml-hide-all-text-content 
        nxml-hide-direct-text-content 
        nxml-hide-other 
        nxml-hide-subheadings 
        nxml-hide-text-content )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory icon."
          (when (interactive-p)
            (emacspeak-auditory-icon 'close-object)
            (emacspeak-speak-line)))))

(loop for f in
      '(nxml-show 
        nxml-show-all 
        nxml-show-direct-subheadings 
        nxml-show-direct-text-content 
        nxml-show-subheadings )
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory icon."
          (when (interactive-p)
            (emacspeak-auditory-icon 'open-object)
            (emacspeak-speak-line)))))
 
;;}}}
;;{{{ Outline summarizer:

(defun emacspeak-nxml-summarize-outline ()
  "Intelligent spoken display of current outline entry."
  (interactive)
  (declare (special o-close))
  (cond
   ((get-text-property (point) 'nxml-outline-state)
    (let ((o-open nil)
          (o-end nil))
      (save-excursion
        (setq o-open (car (overlays-at (point))))
        (next-line 1)
        (beginning-of-line)
        (forward-char -2)
        (setq o-close (car (overlays-at (point))))
        (dtk-speak (concat 
                    (overlay-get  o-open 'display)
                    (overlay-get o-close 'display)))))
    (emacspeak-auditory-icon 'ellipses))
   (t (message "Not on a hidden outline"))))
  
;;}}}
;;}}}
(provide 'emacspeak-nxml)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
