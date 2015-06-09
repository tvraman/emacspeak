;;; emacspeak-bbc.el --- Speech-enabled  BBC client
;;; $Id: emacspeak-bbc.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable BBC An Emacs Interface to bbc
;;; Keywords: Emacspeak,  Audio Desktop bbc
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2015, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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
;;; MERCHANTABILITY or FITNBBC FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; BBC: http://www.bbc.co.uk This module uses
;;; publicly available REST APIs to implement a native Emacs
;;; client for browsing and listening to BBC programs.

;;; See http://www.bbc.co.uk/programmes/developers
;;; The BBC API helps locate a PID for a given program stream.
;;; We then construct the BBC IPlayer URL for that  PID,
;;; And either hand that link off to Chrome,
;;; Or stream it via get_iplayer and mplayer.
;;; get_iplayer: https://github.com/get-iplayer/get_iplayer.git (push)
;;; Pro/Con:
;;; Chrome: Has the UI for seeking in the stream.
;;; get_iplayer: We use a named pipe, and cannot seek,
;;; but the rest of emacspeak-m-player is available.
;;; For downloading a program etc., use package iplayer.
;;; Code:
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'button)
(require 'emacspeak-webutils)
(require 'g-utils)

;;}}}
;;{{{ Helpers:
(defvar emacspeak-bbc-json nil
  "Buffer local variable to store API results.")
(make-variable-buffer-local 'emacspeak-bbc-json)
(defvar emacspeak-bbc-json-schedules-template
  "http://www.bbc.co.uk/%s/programmes/schedules/%s%s.json"
  "URL template for pulling schedules as json.")

(defun emacspeak-bbc-read-schedules-url ()
  "Return URL for schedule for specified station, outlet, date.
Date defaults to today."
  (declare  (special emacspeak-bbc-json-schedules-template
                     emacspeak-bbc-station-list))
  (let* ((fields (split-string
                  (completing-read "Station:Outlet:" emacspeak-bbc-station-list
                                   nil 'must-match)
                  ":"))
         (date (emacspeak-speak-read-date-year/month/date))
         (station (first fields))
         (outlet (second fields)))
    (format emacspeak-bbc-json-schedules-template
            station
            (if  (null outlet) "" (format "%s/" outlet))
            date)))

(defvar emacspeak-bbc-json-genre-template
  "http://www.bbc.co.uk/radio/programmes/genres/%s/schedules.json"
  "Template URL for schedule  by Genre.")

(defun emacspeak-bbc-read-genre-url ()
  "Return URL for specified  genre."
  (declare (special emacspeak-bbc-json-genre-template))
  (let
      ((genre (read-from-minibuffer "Genre/Genre/Genre:")))
    (format emacspeak-bbc-json-genre-template genre)))

(defvar emacspeak-bbc-station-list
  '("radio1"
    "1xtra"
    "radio2"
    "radio3"
    "radio4"
    "radio4extra"
    "5live"
    "5livesportsextra"
    "6music"
    "asiannetwork"
    "worldserviceradio"
    "radioscotland"
    "radionangaidheal"
    "radioulster"
    "radiofoyle"
    "radiowales"
    "radiocymru"
    "radioberkshire"
    "radiobristol"
    "radiocambridgeshire"
    "radiocornwall"
    "bbccoventryandwarwickshire"
    "radiocumbria"
    "radioderby"
    "radiodevon"
    "bbcessex"
    "radiogloucestershire"
    "bbcguernsey"
    "bbcherefordandworcester"
    "radiohumberside"
    "radiojersey"
    "radiokent"
    "radiolancashire"
    "radioleeds"
    "radioleicester"
    "bbclincolnshire"
    "bbclondon"
    "radiomanchester"
    "radiomerseyside"
    "bbcnewcastle"
    "radionorfolk"
    "radionorthampton"
    "radionottingham"
    "bbcoxford"
    "radiosheffield"
    "radioshropshire"
    "radiosolent"
    "bbcsomerset"
    "radiostoke"
    "radiosuffolk"
    "bbcsurrey"
    "bbcsussex"
    "bbctees"
    "threecountiesradio"
    "bbcwiltshire"
    "wm"
    "radioyork"
    "radiolanguagecy"
    "radiolanguagega"
    "radiolanguagegd"
    "http:twitter.comBBCiPlayerRadio" class="radio-link-twitter"
    "radioinfo"
    "http:www.bbc.co.ukblogsradio"
    "http:iplayerhelp.external.bbc.co.ukhelpplaying_radio_progs"
    "radiofeedback"
    "podcasts"
    "radio1"
    "1xtra"
    "radio2"
    "radio3"
    "radio4:lw"
    "radio4:fm"
    "radio4extra"
    "5live"
    "5livesportsextra"
    "6music"
    "asiannetwork"
    "worldserviceradio"
    "radioscotland"
    "radionangaidheal"
    "radioulster"
    "radiofoyle"
    "radiowales"
    "radiocymru")
  "List of BBC Radio stations.")

;;}}}
;;{{{ Stream using get_iplayer:

(defvar emacspeak-bbc-iplayer-handle "/tmp/iplayer-stream.mp3"
  "Name of named pipe used for streaming.")

(defcustom emacspeak-bbc-get-iplayer (executable-find "get_iplayer")
  "Name of get_iplayer executable."
  :type 'string
  :group 'emacspeak-bbc)

(defun emacspeak-bbc-get-iplayer-action (button)
  "Stream using get_iplayer."
  (declare (special emacspeak-bbc-get-iplayer
                    emacspeak-bbc-iplayer-handle))
  (let* ((pid (button-get button 'pid))
         (command
          (format "%s --stream --pid=%s --modes=flashaaclow,hlsaaclow > %s &"
                  emacspeak-bbc-get-iplayer pid emacspeak-bbc-iplayer-handle)))
    (unless (file-exists-p emacspeak-bbc-iplayer-handle)
      (shell-command (format "mknod %s p" emacspeak-bbc-iplayer-handle)))
    (message "Initialized stream, please wait.")
    (shell-command  command " *get-iplayer*")
    (sit-for 1)
    (emacspeak-m-player emacspeak-bbc-iplayer-handle)))

;;}}}
;;{{{ Generic Button Action:

(defcustom emacspeak-bbc-button-action  'get-iplayer
  "Action to use for BBC iPlayer buttons.
get-iplayer: use get_iplayer.
chrome: Hand off URL to Chrome."
  :type '(choice :tag "Player"
                 (const :tag "Use get_iplayer"  get-iplayer)
                 (const :tag "Use chrome"  chrome))
  :group 'emacspeak-bbc)

(defun emacspeak-bbc-iplayer-button-action (button)
  "Generic button action that dispatches to get_iplayer or chrome based
on user preference."
  (declare (special emacspeak-bbc-button-action))
  (ecase emacspeak-bbc-button-action
    ('chrome (funcall #'emacspeak-bbc-chrome-action button))
    ('get-iplayer (funcall #'emacspeak-bbc-get-iplayer-action button))))

;;{{{ Action: Chrome

(defun emacspeak-bbc-chrome-action (button)
  "Play program  refered to by this button."
  (browse-url-chromium (button-get  button 'link)))

;;}}}

;;}}}

;;{{{ BBC IPlayer Interaction

;;;###autoload
(defun emacspeak-bbc (&optional genre)
  "Launch BBC Interaction.
See http://www.bbc.co.uk/radio/stations for full list of stations.
See http://www.bbc.co.uk/radio/programmes/genres for list of genres.
Interactive prefix arg filters  content by genre."
  (interactive "P")
  (cond
   (genre  (call-interactively 'emacspeak-bbc-genre))
   (t (emacspeak-bbc-iplayer (emacspeak-bbc-read-schedules-url)))))

;;;###autoload
(defun emacspeak-bbc-genre ()
  "Launch BBC Interaction for specified Genre."
  (interactive)
  (emacspeak-bbc-iplayer  (emacspeak-bbc-read-genre-url) 'genres))

(defun emacspeak-bbc-iplayer (url &optional genres)
  "Generate BBC IPlayer interface  from JSON."
  (message url)
  (emacspeak-bbc-iplayer-create
   (g-json-get-result
    (format "%s --max-time 5 --connect-timeout 3 %s '%s'"
            g-curl-program g-curl-common-options
            url))
   genres))

(defun emacspeak-bbc-iplayer-create (json &optional genres)
  "Create iplayer buffer given JSON object."
  (declare (special emacspeak-bbc-json))
  (let* ((inhibit-read-only t)
         (title (or
                 (g-json-lookup "schedule.service.title" json)
                 "BBC IPlayer"))
         (buffer (get-buffer-create title)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert title)
      (insert "\n\n")
      (loop
       for show across
       (if genres
           (g-json-get 'broadcasts json)
         (g-json-lookup  "schedule.day.broadcasts" json))
       and position  from 1
       do
       (insert (format "%d\t" position))
       (emacspeak-bbc-insert-show show)
       (insert "\n\n"))
      (goto-char (point-min))
      (emacspeak-webspace-mode)
      (setq emacspeak-bbc-json json))
    (switch-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(define-button-type 'emacspeak-bbc-iplayer-button
  'follow-link t
  'pid nil
  'help-echo "Play Program"
  'action #'emacspeak-bbc-iplayer-button-action)

(defvar emacspeak-bbc-iplayer-uri-prefix
  "http://www.bbc.co.uk/radio/player/"
  "URL end-point for iplayer.")

(defun   emacspeak-bbc-insert-show (show)
  "Insert a formatted button for this show."
  (declare (special emacspeak-bbc-iplayer-uri-prefix))
  (let ((title  (g-json-lookup-string "programme.display_titles.title" show))
        (pid (g-json-lookup-string "programme.pid" show))
        (short-title
         (g-json-lookup-string "programme.display_titles.subtitle" show))
        (start (g-json-get 'start show))
        (synopsis (g-json-lookup-string "programme.short_synopsis" show))
        (orig (point)))
    (when start (setq start (emacspeak-speak-decode-rfc-3339-datetime start)))
    (insert-text-button
     (format "%s %s" title short-title); label
     'type 'emacspeak-bbc-iplayer-button
     'link (concat emacspeak-bbc-iplayer-uri-prefix pid)
     'pid pid)
    (insert (format "\t%s\n" start))
    (insert (format "%s" synopsis))
    (put-text-property  orig (point) 'show show)))

;;}}}
(provide 'emacspeak-bbc)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
