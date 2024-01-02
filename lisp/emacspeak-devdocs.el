;;; emacspeak-devdocs.el --- Speech-enable DEVDOCS  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable DEVDOCS An Emacs Interface to devdocs
;;; Keywords: Emacspeak,  Audio Desktop devdocs
;;;   LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location https://github.com/tvraman/emacspeak
;;;

;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;; DEVDOCS == Browse DevDocs

;;; Code:

;;;   Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  Map Faces:

(voice-setup-add-map 
 '(
   (devdocs-code-block voice-monotone)))

;;;  Interactive Commands:

'(
  devdocs-delete

  devdocs-peruse

  devdocs-update-all
  )

(cl-loop
 for f in 
 '(devdocs-first-page
   devdocs-go-back devdocs-go-forward
   devdocs-goto-page devdocs-goto-target
   devdocs-last-page devdocs-lookup devdocs-peruse
   devdocs-next-entry devdocs-next-page
   devdocs-previous-entry devdocs-previous-page devdocs-search)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

(provide 'emacspeak-devdocs)
;;;  end of file

                                        ; 
                                        ; 
                                        ; 

