;;; emacspeak-webspace.el --- Webspaces In Emacspeak
;;; $Id: emacspeak-webspace.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: WebSpace provides smart updates from the Web.
;;; Keywords: Emacspeak, Audio Desktop webspace
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;; $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; MERCHANTABILITY or FITNWEBSPACE FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction

;;; Commentary:
;;; WEBSPACE == Smart Web Gadgets For The Emacspeak Desktop

;;}}}
;;{{{ Required modules

(require 'cl)
(declaim (optimize (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ring)
(require 'emacspeak-webutils)
(require 'emacspeak-we)
(eval-when '(load)
     (unless (executable-find "xmlstarlet")
       (message "Install xmlstarlet first.")))
;;}}}
;;{{{ WebSpace Display:

(defun emacspeak-webspace-display (infolet)
  "Displays specified infolet.
Infolets use the same structure as mode-line-format and header-line-format.
Generates auditory and visual display."
  (declare (special header-line-format))
  (setq header-line-format infolet)
  (dtk-speak (format-mode-line header-line-format))
  (emacspeak-auditory-icon 'progress))
;;;###autoload
(define-prefix-command 'emacspeak-webspace 'emacspeak-webspace-keymap)

(declaim (special emacspeak-webspace-keymap))
(loop for k in
      '(
        ("w" emacspeak-webspace-weather)
        ("h" emacspeak-webspace-headlines)
        )
      do
      (define-key emacspeak-webspace-keymap (first k) (second k)))

;;}}}
;;{{{ Headlines:
;;{{{ FeedStore

;;;###autoload
(defcustom emacspeak-webspace-feeds
  '(("http://news.com.com/2547-1_3-0-5.xml" rss)
    ("http://news.bbc.co.uk/rss/newsonline_world_edition/south_asia/rss091.xml" rss)
    ("http://news.bbc.co.uk/rss/sportonline_world_edition/front_page/rss091.xml" rss)
    ("http://rss.cnn.com/rss/cnn_world.rss" rss)
    ("http://rss.cnn.com/rss/cnn_us.rss" rss)
    ("http://rss.cnn.com/rss/money_topstories.rss" rss)
    ("http://rss.cnn.com/rss/cnn_tech" rss)
    ("http://rss.cnn.com/rss/cnn_allpolitics.rss" rss)
    ("http://newsrss.bbc.co.uk/rss/newsonline_world_edition/front_page/rss.xml" rss)
 
    ("http://www.google.com/reader/public/atom/user/10949413115399023739/label/officialgoogleblogs" atom)
    ("http://emacspeak.blogspot.com/atom.xml" atom)
    )
  "Collection of ATOM and RSS feeds."
  :type '(repeat
          (list :tag "Feed"
                (string :tag "URL")
                (choice :tag "Type"
                        (const atom :tag "ATOM")
                        (const :tag "RSS" rss))))
  :group  'emacspeak-webspace)

(defsubst emacspeak-webspace-feed-uri (feed)
  "Return URL."
  (first feed))

(defsubst emacspeak-webspace-feed-type (feed)
  "Return type."
  (second feed))

;;}}}

(defvar emacspeak-webspace-atom-xmlns-uri
  "http://www.w3.org/2005/Atom"
  "Atom NS URI.")

(defvar emacspeak-webspace-atom-headlines-template
  (format 
   "xmlstarlet sel --net -N a=%s -t -m a:feed/a:entry/a:title -v . --nl %%s"
   emacspeak-webspace-atom-xmlns-uri)
  "Command line that gives us Atom Feed headlines.")

(defvar emacspeak-webspace-rss-headlines-template
  "xmlstarlet sel --net -t -m //item/title -v . --nl %s"
  "Command line that gives us RSS news headlines.")

(defvar emacspeak-webspace-headlines
  (make-ring
   (* 20 (length emacspeak-webspace-feeds)))
  "Ring of Headlines.")

(defsubst emacspeak-webspace-headlines-fetch ( feed)
  "Add headlines from specified feed to our cache."
  (declare (special emacspeak-webspace-headlines
                    emacspeak-webspace-rss-headlines-template
                    emacspeak-webspace-atom-headlines-template))
  (let ((type (emacspeak-webspace-feed-type feed))
        (uri (emacspeak-webspace-feed-uri feed))
        (template nil))
    (cond
     ((eq type 'atom)
      (setq template emacspeak-webspace-atom-headlines-template))
     ((eq type 'rss)
      (setq template emacspeak-webspace-rss-headlines-template))
     (t (error "Unknown feed type %s" type)))
    (mapc
     #'(lambda (h)
         (unless (zerop (length h))
           (ring-insert emacspeak-webspace-headlines h)))
     (split-string
      (shell-command-to-string (format template feed))
      "\n"))))
 
(defun emacspeak-webspace-headlines-get ()
  "Populate a ring of headlines."
  (mapc 'emacspeak-webspace-headlines-fetch emacspeak-webspace-feeds))

(defvar emacspeak-webspace-headlines-timer nil
  "Timer holding our headlines update timer.")

(defun emacspeak-webspace-update-headlines (period)
  "Setup periodic news updates.
Period is specified as documented in function run-at-time.
Updated headlines found in ring `emacspeak-webspace-headlines"
  (interactive "sUpdate Frequencey: ")
  (declare (special emacspeak-webspace-headlines-timer ))
  (emacspeak-webspace-headlines-get)
  (setq emacspeak-webspace-headlines-timer
        (run-at-time
         period (timer-duration period)
         'emacspeak-webspace-headlines-get)))

(defsubst emacspeak-webspace-next-headline ()
  "Return next headline to display."
  (declare (special emacspeak-webspace-headlines))
  (cond
   ((ring-empty-p emacspeak-webspace-headlines) "No News Is Good News")
   (t
    (let ((h (ring-remove emacspeak-webspace-headlines 0)))
      (ring-insert-at-beginning emacspeak-webspace-headlines h)
      h))))
 
;;;###autoload
(defun emacspeak-webspace-headlines ()
  "Speak current news headline."
  (interactive)
  (declare (special emacspeak-webspace-headlines
                    emacspeak-webspace-headlines-timer))
  (unless emacspeak-webspace-headlines-timer
    (call-interactively 'emacspeak-webspace-update-headlines))
  (emacspeak-webspace-display
   '((:eval (emacspeak-webspace-next-headline)))))
 

;;}}}
;;{{{ Weather:

(defvar emacspeak-webspace-weather-template
  "xmlstarlet sel --net -t -v '//item[1]/title' \
http://www.wunderground.com/auto/rss_full/%s.xml"
  "Command line that gives us weather conditions as a short string.")

(defun emacspeak-webspace-weather-conditions ()
  "Return weather conditions for `emacspeak-url-template-weather-city-state'."
  (declare (special emacspeak-url-template-weather-city-state
                    emacspeak-webspace-weather-template))
  (when (and emacspeak-webspace-weather-template
             emacspeak-url-template-weather-city-state)
    (substring
     (shell-command-to-string
      (format emacspeak-webspace-weather-template
              emacspeak-url-template-weather-city-state))
     0 -1)))

(defvar emacspeak-webspace-current-weather nil
  "Holds cached value of current weather conditions.")

(defvar emacspeak-webspace-weather-timer nil
  "Timer holding our weather update timer.")
(defsubst emacspeak-webspace-weather-get ()
  "Get weather."
  (declare (special emacspeak-webspace-current-weather))
  (setq emacspeak-webspace-current-weather
        (emacspeak-webspace-weather-conditions)))

(defun emacspeak-webspace-update-weather (period)
  "Setup periodic weather updates.
Period is specified as documented in function run-at-time.
Updated weather is found in `emacspeak-webspace-current-weather'."
  (interactive "sUpdate Frequencey: ")
  (declare (special emacspeak-webspace-weather-timer ))
  (unless emacspeak-url-template-weather-city-state
    (error
     "First set option emacspeak-url-template-weather-city-state to your city/state."))
  (emacspeak-webspace-weather-get)
  (setq emacspeak-webspace-weather-timer
        (run-at-time
         period (timer-duration period)
         'emacspeak-webspace-weather-get )))

;;;###autoload
(defun emacspeak-webspace-weather ()
  "Speak current weather."
  (interactive)
  (declare (special emacspeak-webspace-current-weather
                    emacspeak-webspace-weather-timer))
  (unless emacspeak-webspace-weather-timer
    (call-interactively 'emacspeak-webspace-update-weather))
  (emacspeak-webspace-display 'emacspeak-webspace-current-weather))

;;}}}
(provide 'emacspeak-webspace)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
