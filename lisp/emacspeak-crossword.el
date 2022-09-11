;;; emacspeak-crossword.el --- Speech-enable CROSSWORD  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable CROSSWORD An Emacs Interface to crossword
;; Keywords: Emacspeak,  Audio Desktop crossword
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; 
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
;; MERCHANTABILITY or FITNCROSSWORD FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; CROSSWORD == Solve crosswords in Emacs
;; Use fork at https://github.com/ieure/emacs-crossword
;; Rather than the one on melpa.

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
   (crossword-checked-face voice-brighten)
   (crossword-current-face voice-bolden)
   (crossword-error-face voice-monotone)
   (crossword-error-inverse-face voice-animate)
   (crossword-other-dir-face voice-smoothen)
   (crossword-solved-face voice-lighten)))

;;}}}
;;{{{ Interactive Commands:

'(
  crossword
  crossword-backup
  crossword-begin-field
  crossword-bsp-char
  crossword-check-letter
  crossword-check-puzzle
  crossword-check-word
  crossword-clue-scroll-down
  crossword-clue-scroll-page-down
  crossword-clue-scroll-page-end
  crossword-clue-scroll-page-home
  crossword-clue-scroll-page-up
  crossword-clue-scroll-up
  crossword-del-char
  crossword-download
  crossword-end-field
  crossword-goto-clue
  crossword-load
  crossword-mode
  crossword-nav-dir-across
  crossword-nav-dir-down
  crossword-nav-dir-toggle
  crossword-next-char
  crossword-next-field
  crossword-next-line
  crossword-pause-unpause-timer
  crossword-previous-line
  crossword-prior-char
  crossword-prior-field
  crossword-quit
  crossword-restore
  crossword-solve-letter
  crossword-solve-puzzle
  crossword-solve-word
  crossword-summary
  crossword-summary-delete
  crossword-summary-mode
  crossword-summary-revert-buffer
  crossword-summary-select
  crossword-summary-sort
  crossword-summary-tab-backward
  crossword-summary-tab-forward
  )

;;}}}
(provide 'emacspeak-crossword)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
