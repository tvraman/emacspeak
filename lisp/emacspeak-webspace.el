;;; emacspeak-webspace.el --- Webspaces In Emacspeak
;;; $Id$
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
;;; Code:

;;}}}
;;{{{ Required modules

(require 'cl)
(declaim (optimize (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ring)
(require 'derived)
(require 'gfeeds)
(require 'gweb)
(require 'gf)
(require 'emacspeak-webutils)
(require 'emacspeak-feeds)

;;}}}
;;{{{ WebSpace Mode:

;;; Define a derived-mode called WebSpace that is generally useful for hypetext display.
;;;###autoload
(define-derived-mode emacspeak-webspace-mode special-mode
                     "Webspace Interaction"
  "Major mode for Webspace interaction.\n\n
\\{emacspeak-webspace-mode-map}")

(declaim (special emacspeak-webspace-mode-map))
(set-keymap-parent emacspeak-webspace-mode-map button-buffer-map)
(loop for k in
      '(
        ("q" bury-buffer)
        ("." emacspeak-webspace-filter)
        ("'" emacspeak-speak-rest-of-buffer)
        ("<" beginning-of-buffer)
        (">" end-of-buffer)
        ("/" search-forward)
        ("?" search-backward)
        ("y" emacspeak-webspace-yank-link)
        ("n" forward-button)
        ("p" backward-button)
        ("f" forward-button)
        ("b" backward-button))
      do
      (emacspeak-keymap-update emacspeak-webspace-mode-map k))

(defsubst emacspeak-webspace-act-on-link (action &rest args)
  "Apply action to link under point."
  (let ((link (get-text-property (point) 'link)))
    (if link
        (apply action link args)
      (message "No link under point."))))

;;;###autoload
(defun emacspeak-webspace-transcode ()
  "Transcode headline at point by following its link property."
  (interactive)
  (emacspeak-webspace-act-on-link 'emacspeak-webutils-transcode-this-url-via-google))

;;;###autoload
(defun emacspeak-webspace-yank-link ()
  "Yank link under point into kill ring."
  (interactive)
  (let ((button (button-at (point))))
    (cond
     (button (emacspeak-auditory-icon 'yank-object)
             (kill-new
              (or (second (button-get button 'feed))
                  (button-get button 'link))))
     (t (error "No link under point")))))

(defadvice gfeeds-view (around emacspeak pre act comp)
  "Automatically speak display."
  (when (ems-interactive-p )
    (emacspeak-webutils-autospeak))
  ad-do-it
  ad-return-value)

;;;###autoload
(defun emacspeak-webspace-open ()
  "Open headline at point by following its link property."
  (interactive)
  (emacspeak-webspace-act-on-link 'browse-url))

;;;###autoload
(defun emacspeak-webspace-filter ()
  "Open headline at point by following its link property and filter for content."
  (interactive)
  (let ((link (get-text-property (point) 'link)))
    (if link
        (emacspeak-we-xslt-filter
         emacspeak-we-recent-xpath-filter
         link 'speak)
      (message "No link under point."))))

;;}}}
;;{{{ WebSpace Display:

(defsubst emacspeak-webspace-display (infolet)
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
(global-set-key [C-return] 'emacspeak-webspace-headlines-browse)

;;}}}
;;{{{ Headlines:

;;; Encapsulate collection feeds, headlines, timer, and recently updated feed.-index

(defstruct emacspeak-webspace-fs
  feeds
  titles
  timer slow-timer
  index )

(defvar emacspeak-webspace-headlines nil
  "Feedstore structure to use a continuously updating ticker.")
(defvar emacspeak-webspace-headlines-period '(0 1800 0)
  "How often we fetch from a feed.")

(defsubst emacspeak-webspace-headlines-fetch ( feed)
  "Add headlines from specified feed to our cache.
Newly found headlines are inserted into the ring within our feedstore.
We use module gfeeds to efficiently fetch feed contents using the
  Google AJAX API."
  (declare (special emacspeak-webspace-headlines-period))
  (let* ((emacspeak-speak-messages nil)
         (last-update (get-text-property 0 'last-update feed))
         (gfeeds-freshness-internal
          (if last-update
              emacspeak-webspace-headlines-period
            gfeeds-freshness-internal))
         (titles (emacspeak-webspace-fs-titles emacspeak-webspace-headlines)))
    (when                ; check if we need to add from this feed
        (or (null last-update)        ;  at most every half hour
            (time-less-p emacspeak-webspace-headlines-period  (time-since last-update)))
      (put-text-property 0 1 'last-update (current-time) feed)
      (mapc
       #'(lambda (h)
           (unless (ring-member titles h)
             (ring-insert titles h )))
       (gfeeds-titles feed)))))

(defsubst emacspeak-webspace-fs-next (fs)
  "Return next feed and increment index for fs."
  (let ((feed-url (aref
                   (emacspeak-webspace-fs-feeds fs)
                   (emacspeak-webspace-fs-index fs))))
    (setf (emacspeak-webspace-fs-index fs)
          (% (1+ (emacspeak-webspace-fs-index fs))
             (length (emacspeak-webspace-fs-feeds fs))))
    feed-url))

(defun emacspeak-webspace-headlines-populate ()
  "populate fs with headlines from all feeds."
  (declare (special emacspeak-webspace-headlines))
  (dotimes (i (length (emacspeak-webspace-fs-feeds emacspeak-webspace-headlines)))
    (emacspeak-webspace-headlines-fetch (emacspeak-webspace-fs-next emacspeak-webspace-headlines))))

(defsubst emacspeak-webspace-headlines-refresh ()
  "Update headlines."
  (declare (special emacspeak-webspace-headlines))
  (with-local-quit
    (emacspeak-webspace-headlines-fetch
     (emacspeak-webspace-fs-next emacspeak-webspace-headlines)))
  (emacspeak-auditory-icon 'working)
  t)

(defun emacspeak-webspace-headlines-update ()
  "Setup news updates.
Updated headlines found in emacspeak-webspace-headlines."
  (interactive)
  (declare (special emacspeak-webspace-headlines))
  (let ((timer nil)
        (slow-timer nil))
    (setq timer
          (run-with-idle-timer
           60 t 'emacspeak-webspace-headlines-refresh))
    (setq slow-timer
          (run-with-idle-timer
           3600
           t 'emacspeak-webspace-headlines-populate))
    (setf (emacspeak-webspace-fs-timer emacspeak-webspace-headlines) timer)
    (setf (emacspeak-webspace-fs-slow-timer emacspeak-webspace-headlines) slow-timer)))

(defun emacspeak-webspace-next-headline ()
  "Return next headline to display."
  (declare (special emacspeak-webspace-headlines))
  (let ((titles (emacspeak-webspace-fs-titles emacspeak-webspace-headlines)))
    (cond
     ((ring-empty-p titles)
      (emacspeak-webspace-headlines-refresh)
      "No News Is Good News")
     (t (let ((h (ring-remove titles 0)))
          (ring-insert-at-beginning titles h)
          h)))))

;;;###autoload
(defun emacspeak-webspace-headlines ()
  "Startup Headlines ticker."
  (interactive)
  (declare (special emacspeak-webspace-headlines emacspeak-feeds))
  (unless emacspeak-webspace-headlines
    (setq emacspeak-webspace-headlines
          (make-emacspeak-webspace-fs
           :feeds
           (apply
            #'vector
            (delq nil
                  (mapcar
                   #'(lambda (f) (unless (eq  'opml (third f)) (second f)))
                   emacspeak-feeds)))
           :titles (make-ring (* 10 (length emacspeak-feeds)))
           :index 0)))
  (unless (emacspeak-webspace-fs-timer emacspeak-webspace-headlines)
    (call-interactively 'emacspeak-webspace-headlines-update))
  (emacspeak-webspace-display '((:eval (emacspeak-webspace-next-headline)))))

(defvar emacspeak-webspace-headlines-buffer "*Headlines*"
  "Name of buffer that displays headlines.")

(defun emacspeak-webspace-headlines-browse ()
  "Display buffer of browsable headlines."
  (interactive)
  (declare (special emacspeak-webspace-headlines
                    emacspeak-webspace-headlines-buffer))
  (unless emacspeak-webspace-headlines
    (error "No cached headlines in this Emacs session."))
  (with-current-buffer
      (get-buffer-create emacspeak-webspace-headlines-buffer)
    (setq buffer-undo-list t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (insert "Press enter to open stories.\n\n")
      (put-text-property (point-min) (point) 'face font-lock-doc-face)
      (loop
       for h in
       (delq nil (ring-elements (emacspeak-webspace-fs-titles emacspeak-webspace-headlines )))
       and position  from 1
       do
       (insert (format "%d\t" position))
       (emacspeak-webspace-headlines-insert-button h)
       (emacspeak-webspace-mode))))
  (switch-to-buffer emacspeak-webspace-headlines-buffer)
  (goto-char (point-min))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

(define-button-type 'emacspeak-webspace-headline
  'follow-link t
  'link nil
  'help-echo "Open Headline"
  'action #'emacspeak-webspace-headline-action)

(defun emacspeak-webspace-headline-action (button)
  "Open story associated with this button."
  (browse-url (button-get button 'link)))

(defun emacspeak-webspace-headlines-insert-button (headline)
  "Insert a button for this headline at point."
  (insert-text-button
   headline
   'type 'emacspeak-webspace-headline
   'link (get-text-property 0 'link headline )))

;;}}}
;;{{{ Weather:

(defvar emacspeak-webspace-weather-url-template
  "http://www.wunderground.com/auto/rss_full/%s.xml"
  "URL template for weather feed.")

(defun emacspeak-webspace-weather-conditions ()
  "Return weather conditions for `emacspeak-url-template-weather-city-state'."
  (declare (special emacspeak-url-template-weather-city-state
                    emacspeak-webspace-weather-url-template))
  (when (and emacspeak-webspace-weather-url-template
             emacspeak-url-template-weather-city-state)
    (with-local-quit
      (format "%s at %s"
              (first
               (gfeeds-titles
                (format emacspeak-webspace-weather-url-template
                        emacspeak-url-template-weather-city-state)))
              emacspeak-url-template-weather-city-state))))

(defvar emacspeak-webspace-current-weather nil
  "Holds cached value of current weather conditions.")

(defvar emacspeak-webspace-weather-timer nil
  "Timer holding our weather update timer.")
(defun emacspeak-webspace-weather-get ()
  "Get weather."
  (declare (special emacspeak-webspace-current-weather))
  (setq emacspeak-webspace-current-weather
        (emacspeak-webspace-weather-conditions)))

(defun emacspeak-webspace-weather-update ()
  "Setup periodic weather updates.
Updated weather is found in `emacspeak-webspace-current-weather'."
  (interactive)
  (declare (special emacspeak-webspace-weather-timer))
  (unless emacspeak-url-template-weather-city-state
    (error
     "First set option emacspeak-url-template-weather-city-state to your city/state."))
  (emacspeak-webspace-weather-get)
  (setq emacspeak-webspace-weather-timer
        (run-with-idle-timer 600 'repeat
                             'emacspeak-webspace-weather-get )))

;;;###autoload
(defun emacspeak-webspace-weather ()
  "Speak current weather."
  (interactive)
  (declare (special emacspeak-webspace-current-weather
                    emacspeak-webspace-weather-timer))
  (unless emacspeak-webspace-weather-timer
    (call-interactively 'emacspeak-webspace-weather-update))
  (emacspeak-webspace-display 'emacspeak-webspace-current-weather))

;;}}}
;;{{{ Feed Reader:

(defvar emacspeak-webspace-reader-buffer "Reader"
  "Name of Reader buffer.")

;;; New Reader using emacspeak-feeds:

(define-button-type 'emacspeak-webspace-feed-link
  'follow-link t
  'feed nil
  'help-echo "Open Feed"
  'action #'emacspeak-webspace-feed-reader-action)

(defun emacspeak-webspace-feed-reader-insert-button (feed)
  "Insert a button for this feed at point."
  (insert-text-button
   (first feed) ; label
   'type 'emacspeak-webspace-feed-link
   'feed feed))

(defun emacspeak-webspace-feed-reader-action (button)
  "Open feed associated with this button."
  (emacspeak-feeds-browse-feed (button-get button 'feed)))

;;;###autoload
(defun emacspeak-webspace-feed-reader (&optional refresh)
  "Display Feed Reader Feed list in a WebSpace buffer.
Optional interactive prefix arg forces a refresh."
  (interactive "P")
  (declare (special emacspeak-webspace-reader-buffer))
  (when (or refresh
            (not (buffer-live-p (get-buffer emacspeak-webspace-reader-buffer))))
    (emacspeak-webspace-feed-reader-create))
  (switch-to-buffer emacspeak-webspace-reader-buffer)
  (goto-char (point-min))
  (emacspeak-speak-mode-line)
  (emacspeak-auditory-icon 'open-object))
(defun emacspeak-webspace-feed-reader-create ()
  "Prepare Reader buffer."
  (declare (special emacspeak-feeds emacspeak-webspace-reader-buffer))
  (with-current-buffer (get-buffer-create emacspeak-webspace-reader-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (insert "Press enter to open feeds.\n\n")
      (put-text-property (point-min) (point) 'face font-lock-doc-face)
      (loop for f in emacspeak-feeds
            and position  from 1
            do
            (insert (format "%d\t" position))
            (emacspeak-webspace-feed-reader-insert-button f)
            (insert "\n"))
      (switch-to-buffer emacspeak-webspace-reader-buffer)
      (emacspeak-webspace-mode))))

;;;###autoload

;;}}}
;;{{{ Google Search in WebSpace:

(defun emacspeak-webspace-google-save-results(results)
  "Save results in a WebSpace mode buffer for later use."
  (declare (special gweb-history))
  (let ((buffer
         (get-buffer-create (format "Search %s" (first gweb-history))))
        (inhibit-read-only t)
        (headline nil))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-undo-list t)
      (insert (format "Search Results For %s\n\n" (first gweb-history)))
      (center-line)
      (loop
       for r across results
       and i from 1
       do
       (insert (format "%d.\t" i))
       (setq headline
             (or
              (g-html-string (g-json-get 'title r))
              (g-json-get 'titleNoFormatting r)))
       (when headline
         (put-text-property 0 (length headline)
                            'link   (g-json-get 'url r)  headline)
         (emacspeak-webspace-headlines-insert-button headline))
       (insert (format "\n%s\n"
                       (g-html-string (g-json-get 'content r)))))
      (emacspeak-webspace-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (display-buffer buffer)
    (emacspeak-auditory-icon 'open-object)))

;;;###autoload
(defun emacspeak-webspace-google ()
  "Display Google Search in a WebSpace buffer."
  (interactive)
  (let ((gweb-search-results-handler 'emacspeak-webspace-google-save-results))
    (call-interactively 'gweb-google-at-point)))

;;}}}
;;{{{ Freebase:

(define-button-type 'emacspeak-webspace-freebase-topic
  'id nil
  'help-echo ""
  'action #'emacspeak-webspace-freebase-topic-expand )

(defun emacspeak-webspace-freebase-topic-expand (button)
  "Expand topic at point."
  (let* ((inhibit-read-only t)
         (start nil)
         (end nil)
         (id (button-get button 'id))
         (desc (gf-topic-description id)))
    (goto-char (button-end button))
    (insert "\n")
    (put-text-property 0 (length id)
                       'link (get-text-property 0 'link desc) id)
    (emacspeak-webspace-headlines-insert-button id)
    (insert "\n")
    (setq start (point))
    (insert desc)
    (setq end (point))
    (fill-region  start end)
    (emacspeak-speak-region start end)
    (insert "\n")
    (goto-char start)
    (emacspeak-auditory-icon 'open-object)))

(defun emacspeak-webspace-freebase-topic-insert (id)
  "Insert a button for this topic at point."
  (insert-text-button
   id
   'type 'emacspeak-webspace-freebase-topic
   'link (get-text-property 0 'id id   )))

;;;###autoload
(defun emacspeak-webspace-freebase-search (query)
  "Perform a Freebase search and display results."
  (interactive
   (list
    (read-from-minibuffer "Freebase Query: ")))
  (let ((buffer (get-buffer-create (format "Feedbase: %s" query)))
        (start nil)
        (results (gf-search-results (emacspeak-url-encode query)))
        (inhibit-read-only t)
        (title nil)
        (id nil))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-undo-list t)
      (format (buffer-name buffer))
      (center-line)
                                        ; Nuke initial '/' in id 
      (loop
       for r in results
       and i from 1
       do
       (insert (format "%d.\t" i))
       (setq title (first r))
       (setq id (substring (second r) 1))
       (put-text-property 0 (length title)
                          'id id title)
       (emacspeak-webspace-freebase-topic-insert title)
       (insert "\n"))
      (emacspeak-webspace-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'open-object)))

;;}}}
(provide 'emacspeak-webspace)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
