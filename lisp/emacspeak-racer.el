;;; emacspeak-racer.el --- Speech-enable RACER  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable RACER An Emacs Interface to racer
;; Keywords: Emacspeak,  Audio Desktop racer
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;; MERCHANTABILITY or FITNRACER FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; RACER ==  Rust documentation and completion 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:



(voice-setup-add-map 
'(
(racer-help-heading-face voice-lighten)
(racer-tooltip voice-brighten)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(racer-find-definition racer-find-definition-other-frame racer-find-definition-other-window)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

(defadvice racer-describe (after emacspeak pre act comp)
  "speak."
  (when (and  (ems-interactive-p)
              (buffer-live-p (get-buffer "*Racer Help*")))
    (emacspeak-auditory-icon 'help-object)
    (with-current-buffer "*Racer Help*" (emacspeak-speak-buffer))))


;;}}}
(provide 'emacspeak-racer)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
