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
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2003, T. V. Raman
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

(def-voice-font emacspeak-nxml-delimited-data-personality  voice-animate-medium
  'nxml-delimited-data-face
  "Personality for delimited  data."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-name-personality  voice-lock-keyword-personality
  'nxml-name-face
  "Personality used for keywords e.g., names."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-ref-personality  voice-lock-reference-personality
  'nxml-ref-face
  "Personality used for references."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-delimiter-personality  voice-bolden-medium
  'nxml-delimiter-face
  "Personality used for delimiters."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-comment-content-personality  voice-lock-comment-personality
  'nxml-comment-content-face
  "Personality used for comments."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-comment-delimiter-personality
  (list voice-lock-comment-personality voice-smoothen-medium)
  'nxml-comment-delimiter-face
  "Personality used for comment delimiters."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-processing-instruction-delimiter-personality 
  voice-lighten-extra
  'nxml-processing-instruction-delimiter-face
  "Personality used for PI delimiters."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-processing-instruction-target-personality 
  voice-animate-extra
  'nxml-processing-instruction-target-face
  "Personality for PI target."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-processing-instruction-content-personality  voice-animate
  'nxml-processing-instruction-content-face
  "Presonality for PI content."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-cdata-section-delimiter-personality 
  voice-monotone-medium
  'nxml-cdata-section-delimiter-face
  "Personality for CData section delimiters."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-cdata-section-CDATA-personality 
  voice-lock-keyword-personality
  'nxml-cdata-section-CDATA-face
  "Personality used for CData keyword."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-cdata-section-content-personality
  (list voice-monotone voice-smoothen-medium)
  'nxml-cdata-section-content-face
  "Personality used for CData content."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-char-ref-number-personality 
  voice-lock-reference-personality
  'nxml-char-ref-number-face
  "Personality for character references."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-char-ref-delimiter-personality  voice-smoothen
  'nxml-char-ref-delimiter-face
  "Personality used for character ref delimiter."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-entity-ref-name-personality  voice-lock-reference-personality
  'nxml-entity-ref-name-face
  "Personality used for entity references."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-entity-ref-delimiter-personality  voice-smoothen
  'nxml-entity-ref-delimiter-face
  "Personality used for entity ref delimiters."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-tag-delimiter-personality  voice-smoothen
  'nxml-tag-delimiter-face
  "Personality used for angle brackets."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-tag-slash-personality  voice-smoothen-medium
  'nxml-tag-slash-face
  "Personality used for the `/' in closing tags."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-element-prefix-personality  voice-monotone-medium
  'nxml-element-prefix-face
  "Personality used for element prefixes."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-element-colon-personality  voice-monotone
  'nxml-element-colon-face
  "Personality used for `:' in prefixes."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-element-local-name-personality  voice-bolden
  'nxml-element-local-name-face
  "Personality used for local part of element names."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-attribute-prefix-personality  voice-monotone-medium
  'nxml-attribute-prefix-face
  "Personality for attribute prefixes."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-attribute-colon-personality  voice-monotone
  'nxml-attribute-colon-face
  "Personality used for `:' in attribute prefixes."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-attribute-local-name-personality  voice-animate
  'nxml-attribute-local-name-face
  "Personality used for local part of attribute names."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-namespace-attribute-xmlns-personality 
  voice-smoothen-extra
  'nxml-namespace-attribute-xmlns-face
  "Personality used for xmlns."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-namespace-attribute-colon-personality 
  voice-monotone
  'nxml-namespace-attribute-colon-face
  "Personality for `:' in the xmlns declaration."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-namespace-attribute-prefix-personality 
  voice-animate
  'nxml-namespace-attribute-prefix-face
  "Personality for namespace prefix in xmlns declaration."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-attribute-value-personality  voice-lighten
  'nxml-attribute-value-face
  "Personality for attribute values."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-attribute-value-delimiter-personality  voice-smoothen
  'nxml-attribute-value-delimiter-face
  "Personality for attribute value delimiters."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-namespace-attribute-value-personality  voice-lighten
  'nxml-namespace-attribute-value-face
  "personality for namespace attribute value."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-namespace-attribute-value-delimiter-personality  voice-smoothen
  'nxml-namespace-attribute-value-delimiter-face
  "Persnality for namespace attribute value delimiter."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-prolog-literal-delimiter-personality 
  voice-monotone
  'nxml-prolog-literal-delimiter-face
  "Personality for literals in the prologue."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-prolog-literal-content-personality 
  voice-monotone-medium
  'nxml-prolog-literal-content-face
  "Personality for content in the prologue."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-prolog-keyword-personality 
  voice-lock-keyword-personality
  'nxml-prolog-keyword-face
  "Personality for keywords in the prologue."
  :group 'emacspeak-nxml)

(def-voice-font
  emacspeak-nxml-markup-declaration-delimiter-personality 
  voice-smoothen
  'nxml-markup-declaration-delimiter-face
  "Personality for delimiters in markup declarations."
  :group 'emacspeak-nxml)

(def-voice-font emacspeak-nxml-hash-personality  voice-monotone
  'nxml-hash-face
  "Personality for hash marks."
  :group 'emacspeak-nxml)
(def-voice-font emacspeak-rng-error-personality'betty
  'rng-error-face
  "Personality for validation errors."
  :group 'emacspeak-nxml)

;;}}}
;;{{{ pronunciations 

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
       (`
        (defadvice (, f) (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line))))))

(loop for f in 
      '(nxml-balanced-close-start-tag-block
	nxml-finish-element
	nxml-balanced-close-start-tag-inline)
      do
      (eval
       (`
	(defadvice (, f) (after emacspeak pre act comp)
	  "Provide auditory feedback."
	  (when (interactive-p)
	    (emacspeak-auditory-icon 'close-object)
	    (dtk-speak
	     (format "Closed %s"
		     (xmltok-start-tag-qname))))))))

;;}}}
(provide 'emacspeak-nxml)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
