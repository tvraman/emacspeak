;;; emacspeak-forge.el --- Speech-enable FORGE  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable FORGE An Emacs Interface to forge
;;; Keywords: Emacspeak,  Audio Desktop forge
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
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNFORGE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; FORGE ==  Work with Github, Gitlab etc from inside magit.
;;; This module speech-enables magit/forge.

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
   (forge-post-author voice-lighten)
   (forge-post-date voice-animate)
   (forge-topic-closed voice-lighten)
   (forge-topic-merged voice-monotone)
   (forge-topic-open voice-bolden)
   (forge-topic-unmerged voice-animate)
   (forge-topic-unread voice-animate)))

;;}}}
;;{{{ Interactive Commands:

(cl-loop
 for f in 
 '(
   forge-create-issue forge-create-post forge-create-pullreq
   forge-list-issues forge-list-notifications forge-list-pullreqs
   forge-list-visit-issue forge-list-visit-pullreq forge-visit-issue
   forge-visit-pullreq forge-visit-topic)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-line)))))

;;}}}
(provide 'emacspeak-forge)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
