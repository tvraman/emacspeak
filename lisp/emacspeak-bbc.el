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

;;;Copyright (C) 1995 -- 2011, T. V. Raman
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

;;; Commentary: BBC: http://www.bbc.co.uk This module uses
;;; publicly available REST APIs to implement a native Emacs
;;; client for browsing and listening to BBC programs.

;;; See http://www.bbc.co.uk/programmes/developers
;;; The BBC API helps locate a PID for a given program stream.
;;; That PID is converted to a streamable URL via the convertor:
;;; http://www.iplayerconverter.co.uk/convert.aspx
;;;Conversion: http://www.iplayerconverter.co.uk/convert.aspx?pid=%s
;;; The result of the above conversion gives a Web page with a
;;; set of links,
;;; We hand the link to the raw stream  to mplayer.

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

(defvar emacspeak-bbc-iplayer-convertor
  "http://www.iplayerconverter.co.uk/convert.aspx?pid=%s"
  "REST API for converting IPlayer program-id to  stream.")

(defun emacspeak-bbc-get-schedules-url (station outlet date)
  "Return URL for schedule for specified station, outlet, date.
Date defaults to today."
  (interactive
   (list
    (read-from-minibuffer "Station:")
    (read-from-minibuffer "Outlet:")
    (emacspeak-url-template-date-year/month/date)))
  (declare (special emacspeak-bbc-json-schedules-template))
  (format emacspeak-bbc-json-schedules-template
          station
          (if (= (length outlet) 0) "" (format "%s/" outlet))
          date))

;;}}}
;;{{{ BBC IPlayer Interaction
(defun emacspeak-bbc ()
  "Launch BBC Interaction."
  (interactive)
  (emacspeak-bbc-iplayer (call-interactively 'emacspeak-bbc-get-schedules-url)))

(defun emacspeak-bbc-iplayer (url)
  "Generate BBC IPlayer interface  from JSON."
  (message url)
  (emacspeak-bbc-iplayer-create
   (g-json-get-result
    (format "%s --max-time 5 --connect-timeout 3 %s '%s'"
            g-curl-program g-curl-common-options
            url))))

(defun emacspeak-bbc-iplayer-create (json)
  "Create iplayer buffer given JSON object."
  (let ((buffer (get-buffer-create "IPlayer")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (g-json-lookup "schedule.service.title" json))
      (insert "\n\n")
      (loop
       for b in (g-json-lookup  "schedule.day.broadcasts" json)
       and position  from 1
       do
       (insert (format "%d\t" position))
       (emacspeak-bbc-insert-broadcast b)
       (insert "\n"))
      (emacspeak-webspace-mode))
    (switch-to-buffer buffer)))

(define-button-type 'emacspeak-bbc-iplayer-button
  'follow-link t
  'pid nil
  'help-echo "Play Program"
  'action #'emacspeak-bbc-iplayer-button-action)

(defun   emacspeak-bbc-insert-broadcast (broadcast)
  "Insert a formatted button for this broadcast."
  (insert-text-button
   (g-json-lookup "programme.display_titles/title" broadcast) ; label
   'type 'emacspeak-bbc-iplayer-button
   'pid (g-json-lookup "programme.pid" broadcast))
  (insert (g-json-lookup "programme.display_titles.subtitle" broadcast)
          (insert (g-json-get 'start broadcast))
          (insert (g-json-get"programme.short_synopsis" broadcast))))



(defun emacspeak-bbc-iplayer-button-action (button)
  "Play program  refered to by this button."
  (declare (special emacspeak-bbc-iplayer-convertor))
  (add-hook
   'emacspeak-web-post-process-hook
   #'(lambda nil
       (cond
        ((search-forward "mms:" nil t)
         (emacspeak-webutils-play-media-at-point)
         (bury-buffer))
        (t (message "Could not find media link."))))
   'at-end)
  (browse-url
   (format emacspeak-bbc-iplayer-convertor (button-get button 'pid))))

;;}}}
(provide 'emacspeak-bbc)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
