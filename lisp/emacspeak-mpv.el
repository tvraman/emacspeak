;;; emacspeak-mpv.el --- Speech-enable MPV  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MPV An Emacs Interface to mpv
;;; Keywords: Emacspeak,  Audio Desktop mpv
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNMPV FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; MPV ==   Media Player
;; Enables seamless playback of Youtube content among others.
;; Binds mpv commands on C-e C-; as the  prefix key.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile (require 'mpv "mpv" 'no-error))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(
   mpv-kill mpv-pause mpv-play
   mpv-playlist-next mpv-playlist-prev
   mpv-revert-seek mpv-seek mpv-seek-backward mpv-seek-forward
   mpv-seek-to-position-at-point mpv-speed-decrease mpv-speed-increase mpv-speed-set 
   mpv-volume-decrease mpv-volume-increase mpv-volume-set)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Icon."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)))))


;;}}}
;;{{{Keymap:

(defvar emacspeak-mpv-keymap
  (let ((map (make-sparse-keymap)))
    (cl-loop
     for b in
     '(("SPC" mpv-pause)
       (";" mpv-play)
       ("n" mpv-playlist-next)
       ("p" mpv-playlist-prev)
       ("<left>" mpv-seek-backward)
       ("<right>" mpv-seek-forward)
       ("k" mpv-kill)
       ("<up>" mpv-volume-increase)
       ("<down>" mpv-volume-decrease)
       ) do
     (define-key map (ems-kbd (cl-first b)) (cl-second b)))
    map)
  "MPV Keymap")

(define-key emacspeak-keymap (ems-kbd "C-;")  emacspeak-mpv-keymap)
;;}}}
;;{{{repeatable:

(map-keymap
 (lambda (_key cmd)
   (when (symbolp cmd)
     (put cmd 'repeat-map 'emacspeak-mpv-keymap)))
 emacspeak-mpv-keymap)

;;}}}
(provide 'emacspeak-mpv)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
