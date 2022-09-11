;;; emacspeak-hideshow.el --- speech-enable hideshow  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:   extension to speech enable hideshow
;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman<tv.raman.tv@gmail.com>
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;; speech-enable hideshow.el
;;; Code:

;;}}}
;;{{{ speech enable interactive commands 

(defadvice hs-hide-all (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid all blocks.")))
(defadvice hs-show-all (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed all blocks.")))

(defadvice hs-hide-block (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid current block.")))

(defadvice hs-show-block (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed current  block.")))

(defadvice hs-show-region (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Exposed region.")))

(defadvice hs-hide-level (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid all blocks below specified level.")))

(defadvice hs-toggle-hiding (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (cond
     ((hs-already-hidden-p)
      (emacspeak-auditory-icon 'close-object)
      (message "Hid block"))
     (t
      (emacspeak-auditory-icon 'open-object)
      (message "Exposed block")))))

(defadvice hs-hide-initial-comment-block (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Hid initial comment block.")))

;;}}}

(provide 'emacspeak-hideshow)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
