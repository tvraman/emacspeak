;; emacspeak-ciel.el --- Speech-enable CIEL   -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable CIEL An Emacs Interface to ciel
;; Keywords: Emacspeak,  Audio Desktop ciel
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
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;; MERCHANTABILITY or FITNCIEL FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;; Commentary:
;; Package ciel provides vim's "copy inside" and "clear inside" commands.
;; Emacspeak binds these commands to <Super i> and <Super o>.
;; This module speech-enables ciel.

;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice Interactive Commands:

(defadvice ciel-ci (after emacspeak pre act comp)
  "Speech-enabled by emacspeak."
  (when (ems-interactive-p)
    (dtk-speak (car  kill-ring))
    (emacspeak-auditory-icon 'delete-object)))

(defadvice ciel-co (after emacspeak pre act comp)
  "Speech-enabled by emacspeak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (dtk-speak (format "Copied: %s " (car  kill-ring)))))

;;}}}
(provide 'emacspeak-ciel)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
