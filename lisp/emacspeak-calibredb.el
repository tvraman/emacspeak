;;; emacspeak-calibredb.el --- Speech-enable CALIBREDB  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable CALIBREDB An Emacs Interface to calibredb
;;; Keywords: Emacspeak,  Audio Desktop calibredb
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
;;; MERCHANTABILITY or FITNCALIBREDB FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA..

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; CALIBREDB == Browse And Search Local Calibre Library

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'calibredb nil 'no-error)
(require 'emacspeak-epub)
;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (calibredb-archive-face voice-smoothen)
   (calibredb-author-face voice-animate)
   (calibredb-comment-face voice-monotone)
   (calibredb-date-face voice-lighten)
   (calibredb-edit-annotation-header-title-face voice-brighten)
   (calibredb-favorite-face (voice-bolden))
   (calibredb-file-face voice-smoothen)
   (calibredb-format-face voice-monotone)
   (calibredb-highlight-face voice-animate)
   (calibredb-id-face voice-monotone)
   (calibredb-ids-face voice-monotone-extra)
   (calibredb-language-face voice-monotone-medium)
   (calibredb-mark-face voice-lighten-medium)
   (calibredb-pubdate-face voice-lighten)
   (calibredb-publisher-face voice-monotone-medium)
   (calibredb-search-header-highlight-face voice-bolden-medium)
   (calibredb-series-face voice-bolden-medium)
   (calibredb-size-face voice-monotone-medium)
   (calibredb-tag-face voice-annotate)
   (calibredb-title-detail-view-face voice-bolden-medium)
   (calibredb-title-face voice-bolden)))

;;}}}
;;{{{ Advice Interactive Commands:

'(
  calibredb-add
  calibredb-add-dir
  calibredb-add-format
  calibredb-annotation-quit
  calibredb-auto-detect-isbn
  calibredb-capture-at-point
  calibredb-catalog
  calibredb-catalog-bib--transient
  calibredb-catalog-bib-dispatch
  calibredb-clone
  calibredb-convert
  calibredb-convert-to-epub-dispatch
  calibredb-copy-as-org-link
  calibredb-edit-annotation
  calibredb-entry-dispatch
  calibredb-entry-quit
  calibredb-export
  calibredb-fetch-and-set-metadata-by-author-and-title
  calibredb-fetch-and-set-metadata-by-id
  calibredb-fetch-and-set-metadata-by-isbn
  calibredb-filter-by-author-sort
  calibredb-filter-by-book-format
  calibredb-filter-by-last_modified
  calibredb-filter-by-tag
  calibredb-filter-dispatch
  calibredb-find-bib
  calibredb-find-candidate-at-point
  calibredb-library-list
  calibredb-library-next
  calibredb-library-previous
  calibredb-mark-and-forward
  calibredb-mark-at-point
  calibredb-open-dired
  calibredb-org-link-copy
  calibredb-remove
  calibredb-remove-format
  calibredb-remove-marked-items
  calibredb-rga
  calibredb-search-clear-filter
  calibredb-search-live-filter
  calibredb-search-ret
  calibredb-search-toggle-view-refresh
  calibredb-search-update
  calibredb-send-edited-annotation
  calibredb-set-metadata--author_sort
  calibredb-set-metadata--authors
  calibredb-set-metadata--comments
  calibredb-set-metadata--comments-1
  calibredb-set-metadata--list-fields
  calibredb-set-metadata--tags
  calibredb-set-metadata--tags-1
  calibredb-set-metadata--title
  calibredb-set-metadata--transient
  calibredb-set-metadata-dispatch
  calibredb-show-metadata
  calibredb-show-mode
  calibredb-show-next-entry
  calibredb-show-previous-entry
  calibredb-show-refresh
  calibredb-sort-by-author
  calibredb-sort-by-date
  calibredb-sort-by-format
  calibredb-sort-by-id
  calibredb-sort-by-language
  calibredb-sort-by-pubdate
  calibredb-sort-by-size
  calibredb-sort-by-tag
  calibredb-sort-by-title
  calibredb-sort-dispatch
  calibredb-switch-library
  calibredb-tag-mouse-1
  calibredb-toggle-archive-at-point
  calibredb-toggle-favorite-at-point
  calibredb-toggle-highlight-at-point
  calibredb-toggle-order

  calibredb-unmark-and-backward
  calibredb-unmark-and-forward
  calibredb-unmark-at-point
  calibredb-virtual-library-list
  calibredb-virtual-library-next
  calibredb-virtual-library-previous
  calibredb-yank-dispatch
  )

(defadvice calibredb-toggle-view-at-point (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(cl-loop
 for f in
 '(calibredb-view calibredb-show-next-entry calibredb-show-previous-entry)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-predefined-window 1)))))

(defadvice calibredb-search-refresh-and-clear-filter (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice calibredb-search-quit (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in
 '(calibredb-previous-entry calibredb-next-entry)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (emacspeak-speak-line)))))

(defadvice calibredb (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{Emacspeak Commands:

(defun emacspeak-calibredb-epub-eww (&optional broken-ncx)
  "Open EPub at point in EWW.
Optional prefix arg uses alternative renderer that handles epubs
with broken NCX files."
  (interactive "P" )
  (funcall-interactively
   #'emacspeak-epub-eww
   (shell-quote-argument
    (calibredb-getattr (car (calibredb-find-candidate-at-point))
                       :file-path))
   broken-ncx))

;;}}}
;;{{{setup:

(defun emacspeak-calibredb-setup ()
  "Setup Emacspeak for Calibredb."
  (cl-declare (special calibredb-search-mode-map))
  (define-key calibredb-search-mode-map "E" 'emacspeak-calibredb-epub-eww))

(add-hook 'calibredb-search-mode-hook 'emacspeak-calibredb-setup)

;;}}}
(provide 'emacspeak-calibredb)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
