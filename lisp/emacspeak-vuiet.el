;;; emacspeak-vuiet.el --- Speech-enable VUIET  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable VUIET An Emacs Interface to vuiet
;; Keywords: Emacspeak,  Audio Desktop vuiet
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
;; MERCHANTABILITY or FITNVUIET FOR A PARTICULAR PURPOSE.  See the
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
;; VUIET ==  Emacs Music Explorer And Player with last.fm integration
;; This module speech-enables vuiet.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:

(defadvice vuiet-stop (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice vuiet-love-track (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-notify-say "loved strack")))

(defadvice vuiet-unlove-track (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (dtk-notify-say "UnLoved strack")))

(cl-loop
 for f in 
 '(
   vuiet-playing-track-lyrics vuiet-loved-tracks-info
   vuiet-playing-artist-info vuiet-playing-artist-lastfm-page
   vuiet-album-info-search vuiet-artist-info
   vuiet-artist-info-search vuiet-artist-lastfm-page)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

(cl-loop
 for f in 
 '(vuiet-disable-scrobbling vuiet-enable-scrobbling)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon
        (if vuiet-scrobble-enabled 'on 'off))
       (dtk-speak (format "Turned %s scrobbling"
                          (if vuiet-scrobble-enabled "on" "off")))))))

(cl-loop
 for f in 
 '(vuiet-player-volume-inc vuiet-player-volume-dec)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (dtk-notify-say
        (format "Volume %s" (vuiet-player-volume)))))))

;;}}}
;;{{{Additional Commands:
(defun emacspeak-vuiet-track-info ()
  "Speak current playing state."
  (interactive)
  (cl-declare (special mode-line-misc-info))
  (cond
   ((null mode-line-misc-info)
    (dtk-notify-say "Nothing playing on vuiet?") )
   (t
    (dtk-notify-speak (mapconcat #'identity mode-line-misc-info " ")))))

;;}}}

(provide 'emacspeak-vuiet)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
