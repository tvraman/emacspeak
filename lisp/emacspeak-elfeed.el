;;; emacspeak-elfeed.el --- Speech-enable ELFEED, A Feed Reader -*- lexical-binding: t; -*-
;;; $Id: emacspeak-elfeed.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable ELFEED A Feed Reader For Emacs
;;; Keywords: Emacspeak,  Audio Desktop elfeed, Feed Reader
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
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; MERCHANTABILITY or FITNELFEED FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; ELFEED ==  Feed Reader for Emacs.
;;; Install from elpa
;;; M-x package-install  elfeed

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'elfeed "elfeed" 'no-match)
;;}}}
;;{{{ Map Faces to voices

(voice-setup-add-map
 '(
   (elfeed-search-date-face  voice-smoothen)
   (elfeed-search-title-face voice-bolden)
   (elfeed-search-unread-title-face voice-bolden-extra)
   (elfeed-search-feed-face voice-animate)
   (elfeed-search-tag-face voice-lighten)))

;;}}}
;;{{{ Advice interactive commands:

(cl-loop
 for f in
 '(
   elfeed-apply-hooks-now elfeed-search-browse-url elfeed-show-entry elfeed-show-visit
   elfeed-update-feed elfeed-update elfeed-show-refresh
   elfeed-search-update--force elfeed-search-update elfeed-search-untag-all-unread
   elfeed-search-untag-all elfeed-search-tag-all-unread elfeed-search-tag-all
   elfeed-search-show-entry elfeed-load-opml elfeed-export-opml
   elfeed-db-compact elfeed-add-feed
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'task-done)
       (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(elfeed-show-tag elfeed-show-untag)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(defadvice elfeed (after emacspeak pre act  comp)
  "Emacspeak setup."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)))
(cl-loop
 for f in
 '(elfeed-kill-buffer  elfeed-search-quit-window)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act  comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'close-object)
       (emacspeak-speak-mode-line)))))

(defadvice elfeed-search-yank (after emacspeak pre act  comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)))

;;}}}
;;{{{ Helpers:

(defun emacspeak-elfeed-entry-at-point ()
  "Return entry at point."
  (cl-declare (special  elfeed-search--offset elfeed-search-entries))
  (let ((index  (- (line-number-at-pos (point)) elfeed-search--offset)))
    (cond
     ((>= index 0) (nth index elfeed-search-entries))
     (t (error "No entry at point.")))))

(defun emacspeak-elfeed-speak-entry-at-point ()
  "Speak entry at point."
  (interactive)
  (let* ((e (emacspeak-elfeed-entry-at-point))
         (title (and e (elfeed-entry-title e)))
         (tags (and e (elfeed-entry-tags e))))
    (unless e (message "No entry here"))
    (when title
      (dtk-speak (propertize title 'personality voice-brighten))
      (when (memq 'read tags)
        (emacspeak-auditory-icon 'modified-object))
      (when (memq 'seen  tags)
        (emacspeak-auditory-icon 'mark-object))
      (emacspeak-auditory-icon 'item)
      (elfeed-tag e 'seen))))

;;}}}
;;{{{ Define additional interactive commands:

(defun emacspeak-elfeed-next-entry ()
  "Move to next entry and speak it."
  (interactive)
  (forward-line 1)
  (emacspeak-elfeed-speak-entry-at-point))

(defun emacspeak-elfeed-previous-entry ()
  "Move to previous entry and speak it."
  (interactive)
  (forward-line -1)
  (emacspeak-elfeed-speak-entry-at-point))

(defun emacspeak-elfeed-filter-entry-at-point ()
  "Display current article after filtering."
  (interactive)
  (cl-declare (special emacspeak-we-recent-xpath-filter))
  (let* ((entry (emacspeak-elfeed-entry-at-point))
         (link(elfeed-entry-link entry)))
    (when (string=  "" emacspeak-we-recent-xpath-filter)
      (setq emacspeak-we-recent-xpath-filter "//p"))
    (cond
     (entry (elfeed-untag  entry 'unread)
            (emacspeak-we-xslt-filter emacspeak-we-recent-xpath-filter link 'speak))
     (t (message "No link under point.")))))

(defun emacspeak-elfeed-eww-entry-at-point ()
  "Display current article in EWW."
  (interactive)
  (let* ((entry (emacspeak-elfeed-entry-at-point))
         (link(elfeed-entry-link entry)))
    (cond
     (entry (elfeed-untag  entry 'unread)
            (eww link))
     (t (message "No link under point.")))))

;;}}}
;;{{{ Silence warnings/errors
(cl-loop
 for f in
 '(elfeed-update-feed elfeed-handle-parse-error  elfeed-handle-http-error
                      elfeed-unjam elfeed-update)
 do
 (eval
  `(defadvice  ,f (around emacspeak pre act comp)
     "Silence messages and errors."
     (ems-with-errors-silenced ad-do-it))))

;;}}}
;;{{{ Set things up

(defadvice elfeed-search-mode (after emacspeak pre act comp)
  "Set up Emacspeak commands."
  (cl-declare (special elfeed-search-mode-map goal-column))
  (setq goal-column 11)                 ; place point on entry title
  (define-key elfeed-search-mode-map "n" 'emacspeak-elfeed-next-entry)
  (define-key elfeed-search-mode-map "p" 'emacspeak-elfeed-previous-entry)
  (define-key elfeed-search-mode-map "." 'emacspeak-elfeed-filter-entry-at-point)
  (define-key elfeed-search-mode-map [right] 'emacspeak-elfeed-filter-entry-at-point)
  (define-key elfeed-search-mode-map "e" 'emacspeak-elfeed-eww-entry-at-point)
  (define-key elfeed-search-mode-map " "'emacspeak-elfeed-speak-entry-at-point))

;;}}}
(provide 'emacspeak-elfeed)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
