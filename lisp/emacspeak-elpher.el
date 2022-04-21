;;; emacspeak-elpher.el --- Speech-enable ELPHER  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable ELPHER An Emacs Interface to elpher
;; Keywords: Emacspeak,  Audio Desktop elpher
;;{{{  LCD Archive entry:men

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;; MERCHANTABILITY or FITNELPHER FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;; ELPHER ==  gopher/gemini client 
;; Let's see if we can rescue the Content-Oriented Web 
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map 
'(
(elpher-binary voice-monotone)
(elpher-gemini voice-animate-extra)
(elpher-gemini-heading1 voice-lighten)
(elpher-gemini-heading2 voice-brighten)
(elpher-gemini-heading3 voice-smoothen)
(elpher-gemini-preformatted voice-monotone)
(elpher-html voice-bolden)
(elpher-image voice-annotate)
(elpher-index voice-lighten)
(elpher-info voice-monotone)
(elpher-margin-brackets voice-annotate)
(elpher-margin-key voice-lighten)
(elpher-other-url voice-smoothen-extra)
(elpher-search voice-bolden)
(elpher-telnet voice-smoothen-extra)
(elpher-text voice-monotone)
(elpher-unknown voice-annotate)))

;;}}}
;;{{{ Interactive Commands:

'(
elpher-bookmark-current elpher-bookmark-link elpher-bookmarks
elpher-copy-current-url elpher-copy-link-url
elpher-download
elpher-download-current
elpher-info-current
elpher-info-link

elpher-set-gopher-coding-system
elpher-toggle-tls
elpher-unbookmark-current
elpher-unbookmark-link
elpher-view-raw
)

(cl-loop
 for f in 
 '(
   elpher-back elpher-back-to-start elpher elpher-root-dir
               elpher-follow-current-link  elpher-jump
   elpher-go elpher-go-current elpher-reload)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-mode-line)
       (emacspeak-auditory-icon 'open-object)))))


(cl-loop
 for f in 
 '(elpher-prev-link elpher-next-link)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (dtk-speak
        (car (get-text-property (point) 'elpher-page)))))))




;;}}}
(provide 'emacspeak-elpher)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
