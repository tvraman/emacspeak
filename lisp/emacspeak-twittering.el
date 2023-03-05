;;; emacspeak-twittering.el - Speech-enable Twitter -*- lexical-binding: t -*-
;; $Id: emacspeak-twit.el 6133 2009-03-17 02:36:43Z tv.raman.tv $
;; $Author: tv.raman.tv $
;; Description:  Speech-enable twit.el and twitter.el --- Twitter from Emacs
;; Keywords: Emacspeak,  Audio Desktop twittering-mode
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
;; Copyright (C) 1995 -- 2022, T. V. Raman
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
;; MERCHANTABILITY or FITNTWIT FOR A PARTICULAR PURPOSE.  See the
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
;;  module twittering-mode.el  is found on  the emacs wiki
;; This module speech-enables twittering-mode which works using
;;  oauth for authentication.
;; Note: As of Thu Sep  2 08:11:25 PDT 2010
;; twit.el is broken.

;; Advises interactive functions to speak

;;}}}
;;{{{  Required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{ Map->Voice Mappings:

(voice-setup-add-map
 '(
   (twittering-username-face voice-bolden)
   (twittering-uri-face  voice-brighten)))

;;}}}
;;{{{ Advice interactive commands: twittering-mode

(defadvice twittering-toggle-activate-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if twittering-active-mode 'on 'off))
    (message "Turned %s twittering-active-mode"
             (if twittering-active-mode 'on 'off))))

(defadvice twittering-scroll-mode (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon (if twittering-scroll-mode 'on 'off))
    (message "Turned %s twittering-scroll-mode"
             (if twittering-scroll-mode 'on 'off))))
(defadvice twit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-twittering-speak-this-tweet)
    (emacspeak-auditory-icon 'open-object)))

(cl-loop for command in
         '(twittering-goto-next-thing
           twittering-goto-previous-thing)
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "Speak thing moved to."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'mark-object)
               (emacspeak-speak-range)))))

(defun emacspeak-twittering-speak-this-tweet (&optional copy-as-kill)
  "Speak tweet under point.
With interactive prefix arg `copy-as-kill', copy it to kill ring as well."
  (interactive "P")
  (let ((who (get-text-property (point) 'username))
        (what (get-text-property (point) 'text)))
    (cond
     ((and who what)
      (when copy-as-kill (kill-new (format "%s: %s" who what)))
      (dtk-speak (format "%s: %s" who what)))
     (t
      (message "Not on a tweet.")
      (emacspeak-auditory-icon 'warn-user)))))

(cl-loop for command in
         '(twittering-goto-first-status
           twittering-scroll-up
           twittering-scroll-down
           twittering-goto-next-status
           twittering-goto-previous-status
           twittering-goto-next-status-of-user
           twittering-goto-previous-status-of-user
           )
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "Speak status moved to."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'select-object)
               (emacspeak-twittering-speak-this-tweet)))))

(defadvice twittering-edit-post-status (after emacspeak pre act comp)
  "Produce auditory feedback."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
(defadvice twittering-update-status-interactive (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(cl-loop for command in
         '(twittering-friends-timeline
           twittering-replies-timeline
           twittering-user-timeline
           twittering-direct-messages-timeline
           twittering-update-status-interactive
           twittering-search
           twittering-switch-to-previous-timeline
           twittering-switch-to-next-timeline
           twittering-other-user-list-interactive
           twittering-visit-timeline
           twittering-current-timeline
           twittering-view-user-page
           )
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "Provide spoken and auditory feedback."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'news)
               (emacspeak-twittering-speak-this-tweet)))))
(defadvice twittering-kill-buffer (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))
;;}}}>
;;{{{ turn on voice lock:

;; no minor mode hook for now alas:

(add-hook
 'twittering-mode-hook
 #'(lambda ()
     (emacspeak-auditory-icon 'open-object)
     (emacspeak-speak-mode-line)))

;;}}}
;;{{{ Silence chatter

(cl-loop
 for f in
 '(
   twittering-get-and-render-timeline twittering-http-default-sentinel
   twittering-http-post-default-sentinel
   twittering-http-get-default-sentinel
   twittering-retrieve-single-tweet-sentinel)
 do
 (eval
  `(defadvice ,f  (around emacspeak pre act comp)
     "Silence spoken messages while twitter is updating."
     (ems-with-messages-silenced ad-do-it))))

;;}}}
;;{{{ additional interactive commands:

(defvar emacspeak-twittering-protocol-identifier
  (regexp-opt '("http://" "https://"))
  "Match http or https")

(defun emacspeak-twittering-jump-to-following-url ()
  "Move to and open closest URI  following point."
  (interactive)
  (cl-declare (special emacspeak-twittering-protocol-identifier))
  (let ((moved t))
    (while (and moved
                (not (looking-at emacspeak-twittering-protocol-identifier)))
      (setq moved (goto-char (next-single-property-change (point) 'uri))))
    (browse-url-at-point)))

(when (boundp 'twittering-mode-map)
  (cl-declaim (special twittering-mode-map))
  (define-key twittering-mode-map "/" 'twittering-search)
  (define-key
   twittering-mode-map "." 'emacspeak-twittering-jump-to-following-url)
  (define-key twittering-mode-map "," 'emacspeak-twittering-speak-this-tweet)
  )

;;}}}
;;{{{Download: twarc
(defun emacspeak-twittering-twarc (&optional whose)
  "Download data using credentials  for signed-in user.
Interactive prefix arg `whose' prompts for a username whose
timeline we download. "
  (interactive "P")
  (cl-declare (special 
               twittering-oauth-consumer-key 
               twittering-oauth-consumer-secret 
               twittering-oauth-access-token-alist))
  (cl-assert  (executable-find "twarc") t "twarc not installed.")
  (let  ((whose
          (cond
           (whose (read-from-minibuffer "Whose data are we
downloading:"))
           (t 
            (cdr  (assoc "screen_name" twittering-oauth-access-token-alist))))))
    (shell-command
     (format
      "%s \
--consumer_key %s \
--consumer_secret %s \
--access_token %s \
--access_token_secret %s \
 timeline %s > %s.json &"
      (executable-find "twarc")
      twittering-oauth-consumer-key
      twittering-oauth-consumer-secret
      (cdr  (assoc "oauth_token" twittering-oauth-access-token-alist))
      (cdr  (assoc "oauth_token_secret"
                   twittering-oauth-access-token-alist))
      whose whose))
    (message "downloading data in the background.")))

;;}}}
(provide 'emacspeak-twittering)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
