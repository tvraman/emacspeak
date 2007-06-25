;;; emacspeak-gnus.el --- Speech enable GNUS -- Fluent spoken access to usenet
;;; $Id$
;;; $Author: tv.raman.tv $ 
;;; Description:  Emacspeak extension to speech enable Gnus
;;; Keywords: Emacspeak, Gnus, Advice, Spoken Output, News
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;{{{  Introduction:

;;; Commentary

;;; This module advices gnus to speak. 

;;}}}
;;{{{ requires
(require 'emacspeak-preamble)
(require 'gnus)
(require 'gnus-sum)

;;}}}
;;{{{  Customizations:

(defgroup emacspeak-gnus nil
  "Emacspeak customizations for the Gnus News/Mail/RSS reader"
  :group 'emacspeak
  :group 'gnus
  :prefix "emacspeak-gnus-")

(defcustom emacspeak-gnus-punctuation-mode  'some
  "Pronunciation mode to use for gnus buffers."
  :type '(choice
          (const  :tag "Ignore" nil)
          (const  :tag "some" some)
          (const  :tag "all" all))
  :group 'emacspeak-gnus)

;;; These customizations to gnus make it convenient to listen to news:
;;; You can read news mostly by using the four arrow keys.
;;; By default all article headers are hidden, so you hear the real news.
;;; You can expose some of the headers with "T" in summary mode.

;;; Keybindings 
(defun emacspeak-gnus-setup-keys ()
  "Setup Emacspeak keys."
  (declare (special gnus-summary-mode-map
                    gnus-group-mode-map
                    gnus-article-mode-map))
  (when (boundp 'gnus-summary-mode-map)
    (emacspeak-keymap-remove-emacspeak-edit-commands gnus-summary-mode-map))
  (when (boundp 'gnus-article-mode-map)
    (emacspeak-keymap-remove-emacspeak-edit-commands gnus-article-mode-map))
  (when (boundp 'gnus-group-mode-map)
    (emacspeak-keymap-remove-emacspeak-edit-commands gnus-group-mode-map))
  (define-key gnus-summary-mode-map "\C-t" 'gnus-summary-toggle-header)
  (define-key gnus-summary-mode-map "T" 'gnus-summary-hide-all-headers )
  (define-key gnus-summary-mode-map "t"
    'gnus-summary-show-some-headers)
  (define-key gnus-summary-mode-map '[left] 'emacspeak-gnus-summary-catchup-quietly-and-exit)
  (define-key gnus-summary-mode-map '[right] 'gnus-summary-show-article)
  (define-key gnus-group-mode-map "\C-n" 'gnus-group-next-group)
  (define-key gnus-group-mode-map [down] 'gnus-group-next-group)
  (define-key gnus-group-mode-map [up] 'gnus-group-prev-group)
  (define-key gnus-group-mode-map "\C-p" 'gnus-group-prev-group)
  (define-key gnus-summary-wash-map "D" 'gnus-summary-downcase-article)
  (define-key gnus-group-mode-map '[right]
    'gnus-group-read-group))

(add-hook 'gnus-started-hook 'emacspeak-gnus-setup-keys)

;;}}}
;;{{{  Hiding headers

(defvar  gnus-ignored-most-headers
  (concat
   "^Path:\\|^Posting-Version:\\|^Article-I.D.:\\|^Expires:"
   "\\|^Date-Received:\\|^References:\\|^Control:\\|^Xref:"
   "\\|^Lines:\\|^Posted:\\|^Relay-Version:\\|^Message-ID:\\|^Nf-ID:"
   "\\|^Nf-From:\\|^Approved:\\|^Sender:"
   "\\|^Organization:\\|^Approved:\\|^Distribution:\\|^Apparently-To:"
   "\\|^Keywords:\\|^Copyright:\\|^X-Supersedes:\\|^ACategory: \\|^Slugword:"
   "\\|^Priority:\\|^ANPA:\\|^Codes:"
   "\\|^Originator:\\|^Comment:\\|^NNTP-Posting-Host:\\|Original-To:"
   "\\|^Followup-To:\\|^Original-Cc:\\|^Reply-To:")
  "Article headers to ignore when only important article headers are to be
spoken.
See command \\[gnus-summary-show-some-headers].")
(declaim (special gnus-ignored-headers))
(setq gnus-ignored-headers "^.*:")
(declaim (special gnus-visible-headers))
(setq gnus-visible-headers "^Subject:")

(defun gnus-summary-show-some-headers ()
  "Show only the important article headers,
i.e. sender name, and subject."
  (interactive)
  (declare (special gnus-ignored-most-headers )) 
  (let ((gnus-ignored-headers gnus-ignored-most-headers ))
    (gnus-summary-toggle-header 1)
    (gnus-summary-toggle-header -1)))

(defun gnus-summary-hide-all-headers()
  "Hide all headers in the article.
Use this command if you don't want to listen to any article headers when
reading news."
  (interactive)
  (let ((gnus-ignored-headers "^.*:"))
    (gnus-summary-toggle-header 1 )
    (gnus-summary-toggle-header -1)))

;;}}}
;;{{{  helper functions

(defsubst emacspeak-gnus-summary-speak-subject ()
  (emacspeak-dtk-sync)
  (dtk-speak (gnus-summary-article-subject )))

(defsubst emacspeak-gnus-speak-article-body ()
  (declare (special emacspeak-gnus-large-article
                    voice-lock-mode dtk-punctuation-mode))
  (save-excursion
    (set-buffer  "*Article*")
    (goto-char (point-min))
    (setq dtk-punctuation-mode 'some)
    (voice-lock-mode 1)
    (emacspeak-dtk-sync)
    (cond
     ((< (count-lines (point-min) (point-max))
         emacspeak-gnus-large-article)
      (emacspeak-speak-buffer  ))
     (t (emacspeak-auditory-icon 'large-movement )
        (let ((start (point)))
          (move-to-window-line -1)
          (end-of-line)
          (emacspeak-speak-region start (point)))))))

;;}}}
;;{{{ Advise top-level gnus command

;;; emacs can hang if too many message sfly by as gnus starts
(defadvice gnus (around emacspeak pre act )
  "Temporarily deactivate advice on message"
  (dtk-speak  "Starting gnus")
  (let ((emacspeak-speak-messages nil))
    ad-do-it)
  (emacspeak-auditory-icon 'news)
  (message "Gnus is ready "))

(defadvice gnus-group-suspend (after emacspeak pre act com)
  "Provide auditory contextual feedback."
  (when (interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
;;{{{  starting up:

(defadvice gnus-group-post-news (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-get-new-news (around emacspeak pre act )
  "Temporarily deactivate advice on message"
  (dtk-speak  "Getting new  gnus")
  (sit-for 2)
  (let ((emacspeak-speak-messages nil ))
    ad-do-it)
  (message "Gnus is ready ")
  (emacspeak-auditory-icon 'news))

;;}}}
;;{{{  Newsgroup selection

(defadvice gnus-group-select-group (before emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)))

(defadvice gnus-group-first-unread-group (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-read-group  (after  emacspeak pre act)
  "Speak the first article line.
 Produce an auditory icon indicating 
an object has been opened."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak (gnus-summary-article-subject))))

(defadvice gnus-group-prev-group (around emacspeak pre act)
  "Speak the newsgroup line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more newsgroups ")
        (emacspeak-speak-line))))
  ad-return-value)

(defadvice gnus-group-prev-unread-group (around emacspeak pre act)
  "Speak the newsgroup line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more newsgroups ")
        (emacspeak-speak-line))))
  ad-return-value)

(defadvice gnus-group-next-group (around emacspeak pre act)
  "Speak the newsgroup line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    (when (interactive-p) 
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more newsgroups")
        (emacspeak-speak-line)))))

(defadvice gnus-group-next-unread-group (around emacspeak pre act)
  "Speak the newsgroup line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    (when (interactive-p)
      (emacspeak-auditory-icon 'select-object))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more newsgroups")
        (emacspeak-speak-line)))))

(defadvice gnus-group-unsubscribe-current-group (after emacspeak pre act)
  "Produce an auditory icon indicating
this group is being deselected."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-speak-line )))

(defadvice gnus-group-catchup-current (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-yank-group (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'yank-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-get-new-news-this-group  (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice gnus-group-list-groups (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing groups... done")))

(defadvice gnus-topic-mode (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "toggled topic mode")))

(defadvice gnus-article-fill-long-lines (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'modified-object)
    (dtk-speak "wrapped long lines")))

(defadvice gnus-group-list-all-groups (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing all groups... done")))

(defadvice gnus-group-list-all-matching (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing all matching groups... done")))

(defadvice gnus-group-list-killed (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing killed groups... done")))

(defadvice gnus-group-list-matching (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "listing matching groups with unread articles... done")))

(defadvice gnus-group-list-zombies (after emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (dtk-speak "Listing zombie groups... done")))

(defadvice gnus-group-customize (before emacspeak pre act)
  "Provide auditory feedback.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (message "Customizing group %s" (gnus-group-group-name))))

;;}}}
;;{{{  summary mode 

(defadvice gnus-summary-clear-mark-backward  (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value ))

(defadvice gnus-summary-clear-mark-forward  (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value ))

(defadvice gnus-summary-mark-as-dormant (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'mark-object)
          (emacspeak-gnus-summary-speak-subject ))))
    ad-return-value ))

(defadvice gnus-summary-mark-as-expirable (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'mark-object)
          (emacspeak-gnus-summary-speak-subject ))))
    ad-return-value ))

(defadvice gnus-summary-mark-as-processable (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'mark-object)
          (emacspeak-gnus-summary-speak-subject ))))
    ad-return-value ))

(defadvice gnus-summary-unmark-as-processable (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'deselect-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-tick-article-backward (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'mark-object)
          (emacspeak-gnus-summary-speak-subject ))))
    ad-return-value ))

(defadvice gnus-summary-tick-article-forward (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles")
        (progn 
          (emacspeak-auditory-icon 'mark-object)
          (emacspeak-gnus-summary-speak-subject ))))
    ad-return-value ))

(defadvice gnus-summary-delete-article (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon  'delete-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-catchup-from-here (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon  'mark-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-catchup-to-here (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon  'mark-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice  gnus-summary-select-article-buffer (after emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice gnus-summary-prev-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-exit-no-update  (around emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (let ((cur-group gnus-newsgroup-name ))
    ad-do-it
    (when (interactive-p )
      (emacspeak-auditory-icon 'close-object)
      (if (eq cur-group (gnus-group-group-name))
          (dtk-speak "No more unread newsgroups")
        (progn 
          (emacspeak-speak-line))))
    ad-return-value ))

(defadvice gnus-summary-exit  (around emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (let ((cur-group gnus-newsgroup-name ))
    ad-do-it
    (when (interactive-p )
      (emacspeak-auditory-icon 'close-object)
      (if (eq cur-group (gnus-group-group-name))
          (dtk-speak "No more unread newsgroups")
        (progn 
          (emacspeak-speak-line))))
    ad-return-value ))

(defadvice gnus-summary-prev-subject  (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value ))

(defadvice gnus-summary-next-subject  (around  emacspeak pre act)
  "Speak the article  line. 
Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value ))

(defadvice gnus-summary-prev-unread-subject  (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more unread articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value ))

(defadvice gnus-summary-next-unread-subject  (around  emacspeak pre act)
  "Speak the article line.
Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value))

(defadvice gnus-summary-goto-subject (around  emacspeak pre act)
  "Speak the article  line.
 Produce an auditory icon if possible."
  (let ((saved-point (point )))
    ad-do-it
    (when (interactive-p)
      (if (= saved-point (point))
          (dtk-speak "No more articles ")
        (progn 
          (emacspeak-auditory-icon 'select-object )
          (dtk-speak (gnus-summary-article-subject )))))
    ad-return-value ))

(defadvice gnus-summary-catchup-and-exit (after emacspeak pre act)
  "Speak the newsgroup line.
 Produce an auditory icon indicating 
the previous group was closed."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-line )))

  

(defadvice gnus-summary-mark-as-unread-forward (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-mark-as-read-forward (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon'mark-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-mark-as-unread-backward (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-mark-as-read-backward (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'mark-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-kill-same-subject-and-select (after emacspeak pre act)
  "Speak the subject and speak the first screenful.
Produce an auditory icon
indicating the article is being opened."
  (when (interactive-p)
    (emacspeak-gnus-summary-speak-subject)
    (sit-for 2)
    (emacspeak-auditory-icon 'open-object)
    (save-excursion
      (set-buffer  "*Article*")
      (emacspeak-dtk-sync)
      (let ((start  (point ))
            (window (get-buffer-window (current-buffer ))))
        (forward-line (window-height window))
        (emacspeak-speak-region start (point ))))))

(defadvice gnus-summary-kill-same-subject (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-next-thread (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-prev-thread (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-up-thread (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon'select-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-down-thread (after emacspeak pre act)
  "Speak the line. 
Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject )))

(defadvice gnus-summary-kill-thread (after emacspeak pre act)
  "Speak the line.
 Produce an auditory icon if possible."
  (when (interactive-p) 
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-gnus-summary-speak-subject )))

;;}}}
;;{{{  Article reading

(defun emacspeak-gnus-summary-catchup-quietly-and-exit ()
  "Catch up on all articles in current group."
  (interactive)
  (gnus-summary-catchup-and-exit t t)
  (emacspeak-auditory-icon 'close-object))
;;; helper function:

(defvar emacspeak-gnus-large-article 30 
  "*Articles having more than
emacspeak-gnus-large-article lines will be considered to be a large article.
A large article is not spoken all at once;
instead you hear only the first screenful.")

           
           

(defadvice gnus-summary-show-article (after emacspeak pre act)
  "Start speaking the article. "
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-page (after emacspeak pre act)
  "Speak the next pageful "
  (dtk-stop)
  (emacspeak-auditory-icon 'scroll)
  (save-excursion
    (set-buffer  "*Article*")
    (let ((start  (point ))
          (window (get-buffer-window (current-buffer ))))
      (forward-line (window-height window))
      (emacspeak-speak-region start (point )))))

(defadvice gnus-summary-prev-page (after emacspeak pre act)
  "Speak the previous  pageful "
  (dtk-stop)
  (emacspeak-auditory-icon 'scroll)
  (save-excursion
    (set-buffer  "*Article*")
    (let ((start  (point ))
          (window (get-buffer-window (current-buffer ))))
      (forward-line (-  (window-height window)))
      (emacspeak-speak-region start (point )))))

(defadvice gnus-summary-beginning-of-article (after emacspeak pre act)
  "Speak the first line. "(save-excursion
                            (set-buffer "*Article*")
                            (emacspeak-speak-line )))

(defadvice gnus-summary-end-of-article

  (after emacspeak pre act)
  "Speak the first line. "(save-excursion
                            (set-buffer "*Article*")
                            (emacspeak-speak-line )))

(defadvice gnus-summary-next-unread-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-prev-unread-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-prev-same-subject  (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-next-same-subject  (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-first-unread-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body)))

(defadvice gnus-summary-goto-last-article (after emacspeak pre act)
  "Speak the article. "
  (when (interactive-p)
    (emacspeak-gnus-speak-article-body )))

(defadvice gnus-article-show-summary  (after emacspeak pre act)
  "Speak the modeline.
Indicate change of selection with
  an auditory icon if possible."
  (when (interactive-p )
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-mode-line)))

(defadvice gnus-article-next-page (after emacspeak pre act )
  "Speak the current window full of news"
  (when (interactive-p)
    (emacspeak-speak-current-window )))

(defadvice gnus-article-prev-page (after emacspeak pre act )
  "Speak the current window full"
  (when    (interactive-p)
    (emacspeak-speak-current-window)))

(defadvice gnus-article-next-button (after emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (let ((end (next-single-property-change
                (point) 'gnus-callback)))
      (emacspeak-auditory-icon 'large-movement)
      (message (buffer-substring
                (point)end )))))

(defadvice gnus-article-press-button (before emacspeak pre act comp)
  "Provide auditory feedback"
  (when (interactive-p)
    (emacspeak-auditory-icon 'button)))

(defadvice gnus-article-goto-prev-page (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (sit-for 1)
    (emacspeak-speak-current-window)))

(defadvice gnus-article-goto-next-page (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
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
;;{{{ rdc: refreshing the pronunciation  and punctuation mode

(add-hook 'gnus-article-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

(add-hook 'gnus-group-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

;; the following is for summary mode.  By default, the 
;; summary mode hook is defined as gnus-agent-mode

(add-hook 'gnus-agent-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

(add-hook 'gnus-article-edit-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

(add-hook 'gnus-category-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

(add-hook 'gnus-score-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

(add-hook 'gnus-server-mode-hook
          (function (lambda ()
                      (dtk-set-punctuations emacspeak-gnus-punctuation-mode)
                      (emacspeak-pronounce-refresh-pronunciations))))

;;}}}
;;{{{ rdc: mapping font faces to personalities 

;; article buffer personalities

;; Since citation does not normally go beyond 4 levels deep, in my 
;; experience, there are separate voices for the first four levels
;; and then they are repeated
(voice-setup-add-map
 ;; NOTE
 ;; face names are either marked at the end of the line with a comment
 ;; consisting of the emacs version or they are unmarked.  The unmarked 
 ;; face names are for gnus shipped with emacs 22 
 ;; rdc 102206
 '(
   (gnus-cite-face-1 voice-bolden) ;; emacs 21
   (gnus-cite-1 voice-bolden-medium)
   (gnus-cite-face-2 voice-lighten) ;; emacs 21
   (gnus-cite-2 voice-lighten) 
   (gnus-cite-face-3 voice-lighten-extra) ;; emacs 21
   (gnus-cite-3 voice-lighten-extra)
   (gnus-cite-face-4 voice-bolden-medium) ;; emacs 21
   (gnus-cite-4 voice-bolden)
   (gnus-cite-face-5 voice-bolden) ;; emacs 21
   (gnus-cite-5 voice-bolden-medium)
   (gnus-cite-face-6 voice-lighten) ;; emacs 21
   (gnus-cite-6 voice-lighten)
   (gnus-cite-face-7 voice-lighten-extra) ;; emacs 21
   (gnus-cite-7 voice-lighten-extra)
   (gnus-cite-face-8 voice-bolden-medium) ;; emacs 21
   (gnus-cite-8 voice-bolden)
   (gnus-cite-face-9 voice-bolden) ;; emacs 21
   (gnus-cite-9 voice-bolden-medium)
   (gnus-cite-face-10 voice-lighten) ;; emacs 21
   (gnus-cite-10 voice-lighten)
   (gnus-cite-11 voice-lighten-extra)
   (gnus-emphasis-highlight-words voice-lighten-extra)
   (gnus-emphasis-bold voice-bolden-and-animate)
   (gnus-emphasis-strikethru voice-bolden-extra)
   (gnus-emphasis-italic voice-lighten)
   (gnus-emphasis-underline voice-brighten-extra)
   (gnus-signature-face voice-animate) ;; emacs 21
   (gnus-signature voice-animate)
   (gnus-header-content-face voice-bolden) ;; emacs 21
   (gnus-header-content voice-bolden)
   (gnus-header-name-face voice-animate) ;; emacs 21
   (gnus-header-name voice-animate)
   (gnus-header-from-face voice-bolden) ;; emacs 21
   (gnus-header-from voice-bolden)
   (gnus-header-newsgroups-face voice-bolden) ;; emacs 21
   (gnus-header-newsgroups voice-bolden)
   (gnus-header-subject-face voice-bolden) ;; emacs 21
   (gnus-header-subject voice-bolden)
   ;; ;; summary buffer personalities
   ;; since there are so many distinctions, most variations
   ;; on the same thing are given the same voice.  Any user that
   ;; uses low and high interest is sufficiently advanced to change
   ;; the voice to his own preferences
   (gnus-summary-normal-read-face voice-bolden) ;; emacs 21
   (gnus-summary-normal-read voice-bolden)
   (gnus-summary-high-read-face voice-bolden) ;; emacs 21
   (gnus-summary-high-read voice-bolden)
   (gnus-summary-low-read-face voice-bolden) ;; emacs 21
   (gnus-summary-low-read voice-bolden)
   (gnus-summary-normal-ticked-face voice-brighten) ;; emacs 21
   (gnus-summary-normal-ticked voice-brighten-extra)
   (gnus-summary-high-ticked-face voice-brighten) ;; emacs 21
   (gnus-summary-high-ticked voice-brighten-extra)
   (gnus-summary-low-ticked-face voice-brighten) ;; emacs 21
   (gnus-summary-low-ticked voice-brighten-extra)
   (gnus-summary-normal-ancient-face voice-smoothen-extra) ;; emacs 21
   (gnus-summary-normal-ancient voice-smoothen-extra)
   (gnus-summary-high-ancient-face voice-smoothen-extra) ;; emacs 21
   (gnus-summary-high-ancient voice-smoothen-extra)
   (gnus-summary-low-ancient-face voice-smoothen-extra) ;; emacs 21
   (gnus-summary-low-ancient voice-smoothen-extra)
   (gnus-summary-normal-undownloaded-face voice-bolden) ;; emacs 21
   (gnus-summary-normal-undownloaded voice-bolden-and-animate)
   (gnus-summary-high-undownloaded-face voice-bolden-and-animate) ;; emacs 21
   (gnus-summary-high-undownloadedvoice-bolden-and-animate)
   (gnus-summary-low-undownloaded-face voice-bolden) ;; emacs 21
   (gnus-summary-low-undownloaded voice-bolden-and-animate)
   (gnus-summary-low-unread-face voice-bolden-extra) ;; emacs 21
   (gnus-summary-low-unread voice-bolden-medium)
   (gnus-summary-high-unread-face voice-bolden) ;; emacs 21
   (gnus-summary-high-unread voice-brighten-extra)
   (gnus-summary-selected-face voice-animate-extra) ;; emacs 21
   (gnus-summary-selected voice-animate-extra)
   (gnus-summary-cancelled-face voice-bolden-extra) ;; emacs 21
   (gnus-summary-cancelled voice-bolden-extra)

   ;; group buffer personalities
   ;; I think the voice used for the groups in the buffer should be the 
   ;; default voice.  I might ask if there is a call for different voices 
   ;; as they are only necessary if users have persistently visible groups
   ;; in the case of empty groups, and voices for the various levels.
   (gnus-group-mail-1-empty-face default) ;; emacs 21
   (gnus-group-mail-1-empty voice-bolden-extra)
   (gnus-group-mail-1-face default) ;; emacs 21
   (gnus-group-mail-1 default)
   (gnus-group-mail-2-empty-face voice-smoothen-extra) ;; emacs 21
   (gnus-group-mail-2-empty voice-bolden-extra)
   (gnus-group-mail-2-face voice-bolden) ;; emacs 21
   (gnus-group-mail-2 default)
   (gnus-group-mail-3-empty-face voice-bolden) ;; emacs 21
   (gnus-group-mail-3-empty  voice-bolden-extra)
   (gnus-group-mail-3-face voice-bolden) ;; emacs 21
   (gnus-group-mail-3 default)
   (gnus-group-mail-low-empty-face voice-bolden) ;; emacs 21
   (gnus-group-mail-low-empty voice-bolden-extra)
   (gnus-group-mail-low-face voice-bolden) ;; emacs 21
   (gnus-group-mail-low default)
   (gnus-group-news-1-empty-face voice-bolden) ;; emacs 21
   (gnus-group-news-1-empty voice-bolden-extra)
   (gnus-group-news-1-face voice-bolden) ;; emacs 21
   (gnus-group-news-1 default)
   (gnus-group-news-2-empty-face voice-bolden) ;; emacs 21
   (gnus-group-news-2-empty voice-bolden-extra)
   (gnus-group-news-2-face voice-bolden-extra) ;; emacs 21
   (gnus-group-news-2 default)
   (gnus-group-news-3-empty-face voice-bolden) ;; emacs 21
   (gnus-group-news-3-empty voice-bolden-extra)
   (gnus-group-news-3-face voice-bolden) ;; emacs 21
   (gnus-group-news-3 default)
   (gnus-group-news-4-empty-face voice-bolden) ;; emacs 21
   (gnus-group-news-4-empty voice-bolden-extra)
   (gnus-group-news-4-face voice-bolden) ;; emacs 21
   (gnus-group-news-4 default)
   (gnus-group-news-5-empty-face voice-bolden) ;; emacs 21
   (gnus-group-news-5-empty voice-bolden-extra)
   (gnus-group-news-5-face voice-bolden) ;; emacs 21
   (gnus-group-news-5 default)
   (gnus-group-news-6-empty-face voice-bolden) ;; emacs 21
   (gnus-group-news-6-empty voice-bolden-extra)
   (gnus-group-news-6-face voice-bolden-extra) ;; emacs 21
   (gnus-group-news-6 default)
   (gnus-group-news-low-empty-face voice-bolden-extra) ;; emacs 21
   (gnus-group-news-low-empty voice-bolden-extra)
   (gnus-group-news-low-face voice-bolden-extra) ;; emacs 21
   (gnus-group-news-low default)
   
   ;; server buffer personalities

   (gnus-server-agent-face voice-bolden) ;; emacs 21
   (gnus-server-agent voice-bolden)
   (gnus-server-closed-face voice-bolden-medium) ;; emacs 21
   (gnus-server-closed voice-bolden-medium)
   (gnus-server-denied-face voice-bolden-extra) ;; emacs 21
   (gnus-server-denied voice-bolden-extra)
   (gnus-server-offline-face voice-animate) ;; emacs 21
   (gnus-server-offline voice-animate)
   (gnus-server-opened-face voice-lighten) ;; emacs 21
   (gnus-server-opened voice-lighten)
   ))
;;}}}
(provide 'emacspeak-gnus)
;;{{{  end of file 
;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 
;;}}}
