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

;;; Copyright (C) 1995 -- 2006, T. V. Raman<raman@cs.cornell.edu>
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

(require 'emacspeak-preamble)
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
  (dtk-set-punctuations 'all))

(defun emacspeak-xsl-voice-lock-setup()
  "Setup voice locking for xsl mode."
  'no-op)

;;}}}
;;{{{ voice locking 

(defvar xsl-xsl-alternate-personality
  voice-animate
  "Personality used in xsl highlighting."
  :group 'emacspeak-xslide)
(defcustom xsl-fo-alternate-personality voice-monotone 
  "Personality used in XSL highlighting."
  :group 'emacspeak-xslide)

(defcustom xsl-other-element-personality voice-animate
  "Personality used in XSL highlighting."
  :group 'emacspeak-xslide)

(defvar xsl-xsl-main-personality voice-bolden 
  "Personality used for highlighting in XSL.")

;;}}}
(provide 'emacspeak-xslide)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
