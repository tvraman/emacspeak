;;; emacspeak-ytel.el --- Speech-enable YTEL  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable YTEL An Emacs Interface to ytel
;;; Keywords: Emacspeak,  Audio Desktop ytel
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
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
;;; MERCHANTABILITY or FITNYTEL FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; YTEL ==  Youtube From Elisp
;;; Install package ytel from Melpa.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ytel "ytel" 'no-error)
;;}}}
;;{{{ Interactive Commands:

(defvar emacspeak-ytel-yt-url-pattern
  "https://www.youtube.com/watch?v=%s"
  "Youtube URL pattern.")

(defun emacspeak-ytel-play-at-point (id &optional best)
  "Play video. Argument `id' is the video-id.
Play current video in ytel when called interactively.
Optional interactive prefix arg `best' picks best audio format."
  (interactive
   (list
    (ytel-video-id (ytel-get-current-video))
    current-prefix-arg))
  (cl-declare (special emacspeak-ytel-yt-url-pattern))
  (funcall-interactively
   #'emacspeak-m-player-youtube-player
   (format emacspeak-ytel-yt-url-pattern id)
   best))

(defun emacspeak-ytel-download (id )
  "Download video at point."
  (interactive
   (list (ytel-video-id (ytel-get-current-video))))
  (cl-declare (special emacspeak-m-player-youtube-dl
                       emacspeak-ytel-yt-url-pattern))
  (let ((default-directory (expand-file-name "~/Downloads")))
    (shell-command
     (format "%s '%s' & "
             emacspeak-m-player-youtube-dl
             (format emacspeak-ytel-yt-url-pattern id)))))


(defun emacspeak-ytel-yank (id )
  "Yank youtube URL  for video at point."
  (interactive
   (list (ytel-video-id (ytel-get-current-video))))
  (cl-declare (special emacspeak-ytel-yt-url-pattern))
  (kill-new (message (format emacspeak-ytel-yt-url-pattern id))))

(when
    (and (locate-library "ytel")
         (boundp 'ytel-mode-map)
         (keymapp ytel-mode-map))
  (cl-declaim (special ytel-mode-map))
  (define-key  ytel-mode-map (ems-kbd "d") #'emacspeak-ytel-download)
  (define-key  ytel-mode-map (ems-kbd "RET")
    #'emacspeak-ytel-play-at-point)
  (define-key  ytel-mode-map (ems-kbd "y") #'emacspeak-ytel-yank)
  (define-key  ytel-mode-map "." #'emacspeak-ytel-play-at-point))

;;}}}
;;{{{Advice:

(defadvice ytel (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'opten-object)
    (emacspeak-speak-line)))

(defadvice ytel-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice ytel-search (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'task-done)))

;;}}}
(provide 'emacspeak-ytel)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
