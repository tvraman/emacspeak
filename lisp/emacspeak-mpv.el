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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; MPV ==   Media Player
;; Enables seamless playback of Youtube content among others.
;; Binds mpv commands on C-e C-; as the  prefix key.
;; This leverages Emacs' repeat-mode functionality, so successive mpv
;; commands require only one initial ress of C-e C-;.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-google)
(eval-when-compile (require 'mpv "mpv" 'no-error)
                   (require 'url-parse))

;;}}}
;;{{{Helper: yt-url->time-offset:

(defun ems--yt-get-time-offset (url)
  "Get time offset if present from YT URL."
  (cadr
   (assoc
    "t"
    (mapcar
     #'(lambda (s) (split-string s "="))
     (split-string
      (cl-second (split-string
                  (url-filename (url-generic-parse-url url))
                  "?")) "&")))))

;;}}}
;;{{{ Interactive Commands:

(defadvice mpv-start (after emacspeak pre act comp)
  "Set up repeat sentinel"
  (when (and repeat-mode mpv--process)
    (set-process-sentinel mpv--process #'ems--repeat-sentinel)))



(cl-loop
 for f in 
 '(
    mpv-pause mpv-play
   mpv-playlist-next mpv-playlist-prev
   mpv-revert-seek mpv-seek mpv-seek-backward mpv-seek-forward
   mpv-seek-to-position-at-point
   mpv-speed-decrease mpv-speed-increase mpv-speed-set 
   mpv-volume-set)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Icon."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)))))

(defvar emacspeak-mpv-url nil
  "URL being played in mpv.")

(defadvice mpv-kill (before emacspeak pre act comp)
  "Add org integration."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (cl-pushnew
     `(
       ,(format "e-media:%s#%s"
                (cl-first (split-string emacspeak-mpv-url "#"))
                (mpv-get-playback-position))
       "URL")
     org-stored-links)
    (setq emacspeak-mpv-url nil)))

(defadvice mpv-volume-increase (after emacspeak pre act comp)
  "Icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'right)))

(defadvice mpv-volume-decrease (after emacspeak pre act comp)
  "Icon."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'left)))

;;}}}
;;{{{Additional Interactive Commands :

(defun emacspeak-mpv-position ()
  "Show position and duration."
  (interactive)
  (message "%.2f of %.2f"
           (mpv-get-playback-position) (mpv-get-duration)))

;;}}}
;;{{{Keymap:

(declare-function mpv-get-playback-position "mpv" nil)
(declare-function mpv-get-duration "mpv" nil)

(defvar emacspeak-mpv-keymap
  (let ((map (make-sparse-keymap)))
    (cl-loop
     for b in
     '(("SPC" mpv-pause)
       (";" emacspeak-mpv-play-url)
       ("j" emacspeak-mpv-jump)
       ("l" emacspeak-mpv-store-link)
       ("s" mpv-seek)
       ("n" mpv-playlist-next)
       ("p" mpv-playlist-prev)
       ("<left>" mpv-seek-backward)
       ("<right>" mpv-seek-forward)
       ("k" mpv-kill)
       ("<up>" mpv-volume-increase)
       ("<down>" mpv-volume-decrease)
       ("." emacspeak-mpv-position)
       ) do
     (define-key map (ems-kbd (cl-first b)) (cl-second b)))
    map)
  "MPV Keymap")


(define-key emacspeak-keymap (ems-kbd "C-;")  emacspeak-mpv-keymap)
(global-set-key (kbd "s-;") emacspeak-mpv-keymap)

(declare-function emacspeak-eww-read-url "emacspeak-eww" nil)

(defun emacspeak-mpv-store-link ()
  "Store link at current position."
  (interactive)
  (cl-declare (special org-stored-links emacspeak-mpv-url))
  (cl-pushnew
   `(
     ,(format "e-media:%s#%s"
              (cl-first (split-string emacspeak-mpv-url "#"))
              (mpv-get-playback-position))
     "URL")
   org-stored-links)
  (message "Stored link to current play position."))

;;;###autoload
(defun emacspeak-mpv-play-url (url &optional left-channel)
  "Play URL using mpv;  Prefix arg plays on left channel."
  (interactive
   (list (emacspeak-eww-read-url) current-prefix-arg ))
  (cl-declare (special emacspeak-mpv-jump-action
                       emacspeak-mpv-url))
  (when
      (and url
           (stringp url)
           (string-prefix-p (emacspeak-google-result-url-prefix) url))
    (setq url  (emacspeak-google-canonicalize-result-url url)))
  (if left-channel
      (with-environment-variables (("PULSE_SINK" "tts_left"))
        (mpv-play-url url))
    (mpv-play-url url))
  (setq emacspeak-mpv-url (cl-first (split-string url "#" )))
  
  (setq emacspeak-mpv-jump-action
        #'(lambda ()
            (mpv-seek
             (cl-second (split-string url "#"))))))

(defvar emacspeak-mpv-jump-action nil
  "Stores jump action.")

(defun emacspeak-mpv-jump ()
  "Run MPV  jump action."
  (interactive)
  (cl-declare (special mpv--process))
  (when (process-live-p mpv--process)
      (when (and (boundp 'emacspeak-mpv-jump-action)
                 (functionp emacspeak-mpv-jump-action))
        (funcall emacspeak-mpv-jump-action ))))

;;}}}
;;{{{repeatable:

(map-keymap
 (lambda (_key cmd)
   (when (symbolp cmd)
     (put cmd 'repeat-map 'emacspeak-mpv-keymap)))
 emacspeak-mpv-keymap)
;;; Turn off repeat-mode on stop:
(put 'mpv-kill  'repeat-map nil)
;;}}}
(provide 'emacspeak-mpv)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
