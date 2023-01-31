;;; emacspeak-empv.el --- Speech-enable EMPV  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EMPV An Emacs Interface to empv
;;; Keywords: Emacspeak,  Audio Desktop empv
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
;;; MERCHANTABILITY or FITNEMPV FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EMPV ==  Another Emacs Media Player
;; Provides better Youtube integration
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'empv nil t)

;;}}}
;;{{{Interactive Commands:

'(
  empv-copy-path
empv-current-loop-off
empv-current-loop-on
empv-display-current
empv-enqueue
empv-enqueue-last
empv-enqueue-next
empv-exit
empv-file-loop-off
empv-file-loop-on
empv-log-current-radio-song-name
empv-pause
empv-play
empv-play-audio
empv-play-directory
empv-play-file
empv-play-radio
empv-play-random-channel
empv-play-video
empv-playback-speed-down
empv-playback-speed-up
empv-playlist-clear
empv-playlist-loop-off
empv-playlist-loop-on
empv-playlist-next
empv-playlist-prev
empv-playlist-select
empv-playlist-shuffle
empv-playtlist-save-to-file
empv-resume
empv-save-and-exit
empv-seek
empv-set-playback-speed
empv-set-volume
empv-start
empv-toggle
empv-toggle-debug
empv-toggle-event-display
empv-toggle-video
empv-volume-down
empv-volume-up
empv-youtube
empv-youtube-last-results
empv-youtube-playlist
empv-youtube-results-copy-current
empv-youtube-results-enqueue-current
empv-youtube-results-inspect
empv-youtube-results-mode
empv-youtube-results-play-current
empv-youtube-results-play-or-enqueue-current
empv-youtube-results-show-comments
empv-youtube-show-comments
empv-youtube-show-current-comments
empv-youtube-tabulated
empv-youtube-tabulated-last-results
)



;;}}}
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
