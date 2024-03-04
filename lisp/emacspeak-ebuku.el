;;; emacspeak-ebuku.el --- Speech-enable EBUKU  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Keywords: Emacspeak,  Audio Desktop ebuku
;;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;;;   Copyright:

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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.



;;; Commentary:
;; EBUKU ==  Emacs Buku front-end to manage bookmarks.

;;; Code:

;;;   Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'ebuku nil "ebuku")
;;;  Map Faces:




(voice-setup-add-map 
'(
(ebuku-comment-face voice-monotone)
(ebuku-heading-face voice-bolden)
(ebuku-help-face voice-lighten)
(ebuku-tags-face voice-bolden)
(ebuku-title-face voice-animate)
(ebuku-url-face voice-smoothen)
(ebuku-url-highlight-face voice-brighten)))

;;;  Interactive Commands:

;; in fond memory of the past:
;; See obsolete emacspeak-fix-interactive in our attic.

(defadvice ebuku-search (before emacspeak pre act pro comp)
  "Advice prompt to speak"
  (interactive (list (read-char "n,l,r,t"))))

(cl-loop
 for f in 
 '(ebuku-previous-bookmark ebuku-next-bookmark)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'select-object)
       (emacspeak-speak-line)))))

(defadvice ebuku (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-icon 'open-object)))

(provide 'emacspeak-ebuku)
;;;  end of file
