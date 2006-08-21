;;; emacspeak-gnus.el --- Speech enable GNUS -- Fluent spoken access to usenet
;;; $Id$
;;; $Author$ 
;;; Description:  Emacspeak extension to speech enable Gnus
;;; Keywords: Emacspeak, Gnus, Advice, Spoken Output, News
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(eval-when (compile)
  (when (locate-library "gnus")
    (require 'gnus)
    (require 'gnus-sum)))
(eval-when-compile (require 'dtk-speak)
(require 'voice-lock)
(require 'emacspeak-keymap)
(require 'emacspeak-speak)
(require 'emacspeak-sounds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Introduction:

;;; This module advices gnus to speak. 

;;}}}
;;{{{  Customizations:

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
  (let ((emacspeak-speak-messages nil ))
    ad-do-it)
  (message "Gnus is ready ")
  (emacspeak-auditory-icon 'news))

;;}}}
;;{{{  Newsgroup selection

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

;;}}}
;;{{{  summary mode 

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


(defsubst emacspeak-gnus-speak-article-body ()
  (declare (special emacspeak-gnus-large-article
                    voice-lock-mode dtk-punctuation-mode))
  (save-excursion
    (set-buffer  "*Article*")
    (goto-char (point-min))
    (setq dtk-punctuation-mode "some")
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

;;}}}
(provide 'emacspeak-gnus)
;;{{{  end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
