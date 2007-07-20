;;; emacspeak-apt-sources.el --- speech-enable APT's sources.list file editor
;;; Description:  Emacspeak extension to speech-enable editing of sources.list
;;; Keywords: Emacspeak, apt, sources.list
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Initial version: Author: Igor B. Poretsky
;;; <master@goga.energo.ru>
;;; Updated and maintained by $Author: tv.raman.tv $
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

;;; This module speech-enables apt-sources.el
;;; that is included in the debian-el package
;;; and provides a major mode for editing
;;; APT's sources.list file.

;;}}}
;;{{{  Required modules
(require 'emacspeak-preamble)
(require 'arc-mode)
;;}}}
;;{{{ Advice interactive commands to speak.

(defadvice apt-sources-mode (after emacspeak pre act comp)
  "Setup Emacspeak extensions"
  (voice-lock-mode 1)
  (dtk-set-punctuations "all"))

(defadvice apt-sources-previous-source-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice apt-sources-next-source-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice apt-sources-deb-or-src-replicate (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice apt-sources-insert-local-vars (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice apt-sources-new-source (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

(defadvice apt-sources-around-lines (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon
     (if apt-sources-around-lines
         'on
       'off))))

;;}}}
(provide 'emacspeak-apt-sources)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
