;;; emacspeak-gnus.el --- Speech enable GNUS -- Fluent spoken access to imap, usenet  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description:  Emacspeak extension to speech enable Gnus
;;; Keywords: Emacspeak, Gnus, Advice, Spoken Output, News
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com 
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4532 $ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman 
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module advises gnus to speak. 
;;; Updating support in 2014 (Emacspeak is nearly 20 years old)
;;; Updating in 2018 as I switch to gnus as my primary mail interface.
;;; These customizations to gnus make it convenient to listen to news:
;;; You can read news mostly by using the four arrow keys.
;;; By default all article headers are hidden, so you hear the real news.

;;; Code:

;;}}}
;;{{{ requires

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-hide)
(require 'gnus)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gm-nnir) ; for smart GMail search

;;}}}
;;{{{  Customizations:

(defgroup emacspeak-gnus nil
  "Emacspeak customizations for the Gnus News/Mail/RSS reader"
  :group 'emacspeak
  :group 'gnus
  :prefix "emacspeak-gnus-")

(defcustom emacspeak-gnus-punctuation-mode  'all
  "Pronunciation mode to use for gnus buffers."
  :type '(choice
          (const  :tag "Ignore" nil)
          (const  :tag "some" some)
          (const  :tag "all" all))
  :group 'emacspeak-gnus)

(defcustom  emacspeak-gnus-large-article 100
"Articles having more than
emacspeak-gnus-large-article lines will be considered to be a large article.
A large article is not spoken all at once;
instead you hear only the first screenful."
  :type 'integer
  :group 'emacspeak-gnus 
  )

;;; Keybindings 
(defun emacspeak-gnus-setup-keys ()
  "Setup Emacspeak keys."
  (cl-declare (special gnus-summary-mode-map
                       gnus-group-mmode-map
                       gnus-article-mode-map))
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map "t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map '[left] 'emacspeak-gnus-summary-catchup-quietly-and-exit)
  (define-key gnus-summary-mode-map '[right] 'gnus-summary-show-article)
  ;(define-key  gnus-group-mode-map "?" 'gm-nnir-group-make-nnir-group)
  (define-key gnus-group-mode-map "/" 'gm-nnir-group-make-gmail-group)
  (define-key gnus-group-mode-map ";" 'emacspeak-gnus-personal-gmail-recent)
  (define-key gnus-group-mode-map ":" 'emacspeak-gnus-personal-gmail-last-week)
  (define-key gnus-group-mode-map "\C-n" 'gnus-group-next-group)
  (define-key gnus-group-mode-map [down] 'gnus-group-next-group)
  (define-key gnus-group-mode-map [up] 'gnus-group-prev-group)
  (define-key gnus-group-mode-map "\C-p" 'gnus-group-prev-group)
  (define-key gnus-summary-wash-map "D" 'gnus-summary-downcase-article)
  (define-key gnus-group-mode-map '[right]
    'gnus-group-read-group))

(add-hook 'gnus-started-hook 'emacspeak-gnus-setup-keys)

;;}}}
;;{{{  helper functions

(defun emacspeak-gnus-summary-speak-subject ()
  (dtk-speak (gnus-summary-article-subject)))

(defun emacspeak-gnus-speak-article-body ()
  (cl-declare (special emacspeak-gnus-large-article
                       voice-lock-mode dtk-punctuation-mode
                       gnus-article-buffer))
  (with-current-buffer gnus-article-buffer
    (goto-char (point-min))
    (search-forward "\n\n")
    (cond
     ((< (count-lines (point) (point-max))
         emacspeak-gnus-large-article)
      (emacspeak-speak-rest-of-buffer))
     (t (emacspeak-auditory-icon 'large-movement)
        (let ((start (point))
              (window (get-buffer-window (current-buffer))))
          (with-selected-window window
            (save-excursion
              (move-to-window-line -1)
              (end-of-line)
              (emacspeak-speak-region start (point)))))))))

;;}}}
;;{{{ Advise top-level gnus command

;;; emacs can hang if too many message sfly by as gnus starts
(defadvice gnus (around emacspeak pre act comp)
  "Silence messages, produce auditory icon."
  (dtk-speak  "Starting gnus")
  (ems-with-messages-silenced ad-do-it)
  (emacspeak-auditory-icon 'news)
  (message "Gnus is ready "))
(cl-loop
 for f in
 '(gnus-group-suspend gnus-group-quit
                      gnus-group-exit gnus-server-exit)
 do
 (eval
  `(defadvice ,f(after emacspeak pre act com)
    "Provide auditory contextual feedback."
    (when (ems-interactive-p)
      (emacspeak-speak-mode-line)
      (emacspeak-auditory-icon 'close-object)))))

;;}}}
;;{{{  starting up:

(defadvice gnus-group-post-news (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-get-new-news (around emacspeak pre act comp)
  "Temporarily silence on message"
  (dtk-speak  "Getting new  gnus")
  (ems-with-messages-silenced ad-do-it)
  (message "Gnus is ready ")
  (emacspeak-auditory-icon 'news))

(defadvice nnheader-message-maybe (around emacspeak pre act comp)
  "Silence emacspeak"
  (ems-with-messages-silenced ad-do-it))

;;}}}
;;{{{  Newsgroup selection

(cl-loop
 for f in
 '(
   gnus-group-select-group gnus-group-first-unread-group
   gnus-group-read-group
   gnus-group-prev-group gnus-group-next-group
   gnus-group-prev-unread-group  gnus-group-next-unread-group
   gnus-group-get-new-news-this-group)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(defadvice gnus-group-unsubscribe-current-group (after emacspeak pre act comp)
  "Produce an auditory icon indicating
this group is being deselected."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-catchup-current (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-yank-group (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-list-groups (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing groups... done")))

(defadvice gnus-topic-mode (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "toggled topic mode")))

(defadvice gnus-article-fill-long-lines (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'modified-object)
    (dtk-speak "wrapped long lines")))

(defadvice gnus-group-list-all-groups (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing all groups... done")))

(defadvice gnus-group-list-all-matching (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing all matching groups... done")))

(defadvice gnus-group-list-killed (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing killed groups... done")))

(defadvice gnus-group-list-matching (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-notify-speak "listing matching groups with unread articles... done")))

(defadvice gnus-group-list-zombies (after emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing zombie groups... done")))

(defadvice gnus-group-customize (before emacspeak pre act comp)
  "speak.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Customizing group %s" (gnus-group-group-name))))

;;}}}
;;{{{  summary mode 
(cl-loop
 for f in
 '(
   gnus-summary-clear-mark-backward gnus-summary-clear-mark-forward
   gnus-summary-mark-as-dormant gnus-summary-mark-as-expirable
   gnus-summary-mark-as-processable
   gnus-summary-tick-article-backward gnus-summary-tick-article-forward
   ) do
 (eval
  `(defadvice   ,f (around  emacspeak pre act comp)
     "Speak the article  line.
 Produce an auditory icon if possible."
     (let ((saved-point (point)))
       ad-do-it
       (when (ems-interactive-p)
         (if (= saved-point (point))
             (dtk-notify-speak "No more articles")
           (progn 
             (emacspeak-auditory-icon 'mark-object)
             (emacspeak-gnus-summary-speak-subject))))
       ad-return-value))))

(defadvice gnus-summary-unmark-as-processable (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-delete-article (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon  'delete-object)
    (emacspeak-gnus-summary-speak-subject)))
(cl-loop
 for f in
 '(
   gnus-summary-catchup-to-here gnus-summary-catchup-from-here
   ) do
 (eval
  `(defadvice  ,f (after emacspeak pre act comp)
     "Speak the line.
 Produce an auditory icon if possible."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon  'mark-object)
       (emacspeak-gnus-summary-speak-subject)))))

(defadvice  gnus-summary-select-article-buffer (after emacspeak pre act comp)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice gnus-summary-prev-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-exit-no-update  (around emacspeak pre act comp)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (let ((cur-group gnus-newsgroup-name))
    ad-do-it
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'close-object)
      (if (eq cur-group (gnus-group-group-name))
          (dtk-notify-speak "No more unread newsgroups")
        (progn 
          (emacspeak-speak-line))))
    ad-return-value))

(defadvice gnus-summary-exit  (around emacspeak pre act comp)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (let ((cur-group gnus-newsgroup-name))
    ad-do-it
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'close-object)
      (if (eq cur-group (gnus-group-group-name))
          (dtk-notify-speak "No more unread newsgroups")
        (progn 
          (emacspeak-speak-line))))
    ad-return-value))

(defadvice gnus-summary-prev-subject  (around  emacspeak pre act comp)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (ems-interactive-p)
      (if (= saved-point (point))
          (dtk-notify-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))
    ad-return-value))

(defadvice gnus-summary-next-subject  (around  emacspeak pre act comp)
  "Speak the article  line. 
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (ems-interactive-p)
      (if (= saved-point (point))
          (dtk-notify-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))
    ad-return-value))

(defadvice gnus-summary-prev-unread-subject  (around  emacspeak pre act comp)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (ems-interactive-p)
      (if (= saved-point (point))
          (dtk-notify-speak "No more unread articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))
    ad-return-value))

(defadvice gnus-summary-next-unread-subject  (around  emacspeak pre act comp)
  "Speak the article line.
Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (ems-interactive-p)
      (if (= saved-point (point))
          (dtk-notify-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))
    ad-return-value))

(defadvice gnus-summary-goto-subject (around  emacspeak pre act comp)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point)))
    ad-do-it
    (when (ems-interactive-p)
      (if (= saved-point (point))
          (dtk-notify-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object)
          (dtk-speak (gnus-summary-article-subject)))))
    ad-return-value))

(defadvice gnus-summary-catchup-and-exit (after emacspeak pre act comp)
  "Speak the newsgroup line.
 Produce an auditory icon indicating 
the previous group was closed."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice gnus-summary-mark-as-unread-forward (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-mark-as-read-forward (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-mark-as-unread-backward (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-mark-as-read-backward (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-kill-same-subject-and-select (after emacspeak pre act comp)
  "Speak the subject and speak the first screenful.
Produce an auditory icon
indicating the article is being opened."
  (cl-declare (special gnus-article-buffer))
  (when (ems-interactive-p)
    (emacspeak-gnus-summary-speak-subject)
    (sit-for 2)
    (emacspeak-auditory-icon 'open-object)
    (with-current-buffer
        gnus-article-buffer
      (let ((start  (point))
            (window (get-buffer-window (current-buffer))))
        (with-selected-window window
          (save-excursion
            (move-to-window-line -1)
            (end-of-line)
            (emacspeak-speak-region start (point))))))))

(defadvice gnus-summary-kill-same-subject (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-next-thread (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-prev-thread (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-up-thread (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-down-thread (after emacspeak pre act comp)
  "Speak the line. 
Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))

(defadvice gnus-summary-kill-thread (after emacspeak pre act comp)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (ems-interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject)))
(defadvice gnus-summary-hide-all-threads (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

;;}}}
;;{{{  Article reading

(defun emacspeak-gnus-summary-catchup-quietly-and-exit ()
  "Catch up on all articles in current group."
  (interactive)
  (gnus-summary-catchup-and-exit t t)
  (emacspeak-auditory-icon 'close-object)
  (emacspeak-speak-line))

(defadvice gnus-summary-show-article (after emacspeak pre act comp)
  "Start speaking the article. "
  (when (ems-interactive-p)
    (with-current-buffer gnus-article-buffer
      (emacspeak-auditory-icon 'open-object)
      (condition-case nil 
          (emacspeak-hide-all-blocks-in-buffer)
        (error nil))
      (emacspeak-gnus-speak-article-body))))

(defadvice gnus-summary-next-page (after emacspeak pre act comp)
  "Speak the next pageful "
  (cl-declare (special gnus-article-buffer))
  (dtk-stop)
  (emacspeak-auditory-icon 'scroll)
  (with-current-buffer
      gnus-article-buffer
    (let ((start  (point))
          (window (get-buffer-window (current-buffer))))
      (with-selected-window window
        (save-excursion
          (move-to-window-line -1)
          (end-of-line)
          (emacspeak-speak-region start (point)))))))

(defadvice gnus-summary-prev-page (after emacspeak pre act comp)
  "Speak the previous  pageful "
  (cl-declare (special gnus-article-buffer))
  (dtk-stop)
  (emacspeak-auditory-icon 'scroll)
  (save-current-buffer
    (set-buffer  gnus-article-buffer)
    (let ((start  (point))
          (window (get-buffer-window (current-buffer))))
      (with-selected-window window
        (save-excursion
          (move-to-window-line -1)
          (end-of-line)
          (emacspeak-speak-region start (point)))))))

(defadvice gnus-summary-beginning-of-article (after emacspeak pre act comp)
  "Speak the first line. "
  (cl-declare (special gnus-article-buffer))
  (save-current-buffer
    (set-buffer gnus-article-buffer)
    (emacspeak-speak-line)))

(defadvice gnus-summary-end-of-article

    (after emacspeak pre act comp)
  "Speak the first line. "
  (cl-declare (special gnus-article-buffer))
  (save-current-buffer
    (set-buffer gnus-article-buffer)
    (emacspeak-speak-line)))

(defadvice gnus-summary-next-unread-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-prev-unread-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-prev-same-subject  (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-same-subject  (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-first-unread-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-goto-last-article (after emacspeak pre act comp)
  "Speak the article. "
  (when (ems-interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-article-show-summary  (after emacspeak pre act comp)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice gnus-article-next-page (after emacspeak pre act comp)
  "Speak the current window full of news"
  (when (ems-interactive-p)
    (emacspeak-speak-current-window)))

(defadvice gnus-article-prev-page (after emacspeak pre act comp)
  "Speak the current window full"
  (when    (ems-interactive-p)
    (emacspeak-speak-current-window)))

(defadvice gnus-article-next-button (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (let ((end (next-single-property-change
                (point) 'gnus-callback)))
      (emacspeak-auditory-icon 'large-movement)
      (message (buffer-substring
                (point)end)))))

(defadvice gnus-article-press-button (before emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice gnus-article-goto-prev-page (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (sit-for 1)
    (emacspeak-speak-current-window)))

(defadvice gnus-article-goto-next-page (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (sit-for 1)
    (emacspeak-speak-current-window)))

(defun gnus-summary-downcase-article ()
  "Downcases the article body
Helps to prevent words from being spelled instead of spoken."
  (interactive)
  (gnus-summary-select-article-buffer)
  (article-goto-body)
  (let ((beg (point))
        (end (point-max))
        (inhibit-read-only t))
    (downcase-region beg end))
  (gnus-article-show-summary)
  (emacspeak-auditory-icon 'modified-object)
  (dtk-speak "Downcased article body"))

;;}}}
;;{{{ refreshing the pronunciation  and punctuation mode

(cl-loop
 for hook  in 
 '(
   gnus-article-mode-hook gnus-group-mode-hook gnus-summary-mode-hook
   gnus-agent-mode-hook  gnus-article-edit-mode-hook
   gnus-server-mode-hook gnus-category-mode-hook
   )
 do
 (add-hook
  hook 
  #'(lambda ()
      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
      (emacspeak-pronounce-refresh-pronunciations))))

;;}}}
;;{{{ rdc: mapping font faces to personalities 

;; article buffer personalities

;; Since citation does not normally go beyond 4 levels deep, in my 
;; experience, there are separate voices for the first four levels
;; and then they are repeated
(voice-setup-add-map
 '(
   (gnus-button voice-bolden)
   (gnus-cite-attribution voice-smoothen)
   (gnus-emphasis-bold-italic voice-bolden-medium)
   (gnus-emphasis-underline-bold voice-lighten) 
   (gnus-emphasis-underline-bold-italic  voice-lighten-extra)
   (gnus-emphasis-underline-italic voice-lighten)
   (gnus-group-mail-1  voice-brighten)
   (gnus-group-mail-2  voice-animate)
   (gnus-group-mail-3  voice-lighten)
   (gnus-group-mail-low voice-smoothen)
   (gnus-group-news-1  voice-animate) 
   (gnus-group-news-2  voice-lighten)
   (gnus-group-news-3  voice-brighten)
   (gnus-group-news-4  voice-lighten)
   (gnus-group-news-5 voice-smoothen) 
   (gnus-group-news-6 voice-annotate)
   (gnus-group-news-low  voice-smoothen-extra)
   (gnus-server-cloud  voice-animate)
   (gnus-server-cloud-host  voice-lighten)
   (gnus-splash  voice-brighten) 
   (gnus-summary-high-undownloaded voice-animate)
   (gnus-summary-normal-unread  voice-bolden)
   (gnus-x-face voice-monotone-extra)
   (gnus-cite-1 voice-bolden-medium)
   (gnus-cite-2 voice-lighten) 
   (gnus-cite-3 voice-lighten-extra)
   (gnus-cite-4 voice-smoothen)
   (gnus-cite-5 voice-smoothen-extra)
   (gnus-cite-6 voice-lighten)
   (gnus-cite-7 voice-lighten-extra)
   (gnus-cite-8 voice-bolden)
   (gnus-cite-9 voice-bolden-medium)
   (gnus-cite-10 voice-lighten)
   (gnus-cite-11 voice-lighten-extra)
   (gnus-emphasis-highlight-words voice-lighten-extra)
   (gnus-emphasis-bold voice-bolden-and-animate)
   (gnus-emphasis-strikethru voice-bolden-extra)
   (gnus-emphasis-italic voice-lighten)
   (gnus-emphasis-underline voice-brighten-extra)
   (gnus-signature voice-animate)
   (gnus-header-content voice-bolden)
   (gnus-header-name voice-animate)
   (gnus-header-from voice-bolden)
   (gnus-header-newsgroups voice-bolden)
   (gnus-header-subject voice-bolden)
   ;; ;; summary buffer personalities
   ;; since there are so many distinctions, most variations
   ;; on the same thing are given the same voice.  Any user that
   ;; uses low and high interest is sufficiently advanced to change
   ;; the voice to his own preferences
   (gnus-summary-normal-read voice-smoothen)
   (gnus-summary-high-read voice-bolden)
   (gnus-summary-low-read voice-bolden)
   (gnus-summary-normal-ticked voice-brighten-extra)
   (gnus-summary-high-ticked voice-brighten-extra)
   (gnus-summary-low-ticked voice-brighten-extra)
   (gnus-summary-normal-ancient voice-smoothen-extra)
   (gnus-summary-high-ancient voice-smoothen-extra)
   (gnus-summary-low-ancient voice-smoothen-extra)
   (gnus-summary-normal-undownloaded voice-bolden-and-animate)
   (gnus-summary-low-undownloaded voice-bolden-and-animate)
   (gnus-summary-low-unread voice-bolden-medium)
   (gnus-summary-high-unread voice-brighten-extra)
   (gnus-summary-selected voice-animate-extra)
   (gnus-summary-cancelled voice-bolden-extra)
   ;; group buffer personalities
   ;; I think the voice used for the groups in the buffer should be the 
   ;; default voice.  I might ask if there is a call for different voices 
   ;; as they are only necessary if users have persistently visible groups
   ;; in the case of empty groups, and voices for the various levels.
   (gnus-group-mail-1-empty voice-monotone-extra)
   (gnus-group-mail-2-empty voice-monotone-extra)
   (gnus-group-mail-3-empty  voice-monotone-extra)
   (gnus-group-mail-low-empty voice-monotone-extra)
   (gnus-group-news-1-empty voice-monotone-extra)
   (gnus-group-news-2-empty voice-monotone-extra)
   (gnus-group-news-3-empty voice-monotone-extra)
   (gnus-group-news-4-empty voice-monotone-extra)
   (gnus-group-news-5-empty voice-monotone-extra)
   (gnus-group-news-6-empty voice-monotone-extra)
   (gnus-group-news-low-empty voice-monotone-extra)
   ;; server buffer personalities
   (gnus-server-agent voice-bolden)
   (gnus-server-closed voice-bolden-medium)
   (gnus-server-denied voice-bolden-extra)
   (gnus-server-offline voice-animate)
   (gnus-server-opened voice-lighten)))

;;}}}
;;{{{ server mode:

(cl-loop
 for f in 
 '(gnus-server-edit-buffer gnus-group-enter-server-mode gnus-browse-exit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)))))

;;}}}
;;{{{ Async Gnus:

;;}}}
;;{{{ GMail Search Accelerators:


(defun emacspeak-gnus-personal-gmail-recent ()
  "Look for mail addressed personally in the last day."
  (interactive)
  (gm-nnir-group-make-gmail-group
   (format "newer_than:1d to:me -cc:%s" user-mail-address)))


(defun emacspeak-gnus-personal-gmail-last-week()
  "Look for mail addressed personally in the last week."
  (interactive)
  (gm-nnir-group-make-gmail-group
   (format
    "after:%s before:%s to:me -cc:%s"
    (format-time-string "%Y/%m/%d" (time-subtract (current-time) (* 7 86400)))
    (format-time-string "%Y/%m/%d")
    user-mail-address)))

;;}}}
(provide 'emacspeak-gnus)
;;{{{  end of file 
;;; local variables:
;;; folded-file: t
;;; end: 
;;}}}
