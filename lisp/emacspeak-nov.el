;;; emacspeak-nov.el --- Speech-enable NOV  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable NOV An Emacs Interface to nov
;;; Keywords: Emacspeak,  Audio Desktop nov
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
;;; MERCHANTABILITY or FITNNOV FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; NOV == Yet Another EPub Reader 
;;; Package nov.el is an alternative to Emacspeak's built-in EPub
;;; reader.
;;; This module speech-enables nov.el

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Interactive Commands:


(cl-loop
 for f in 
 '(
  nov-browse-url
nov-display-metadata
nov-goto-toc
nov-next-document
nov-previous-document
)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (emacspeak-speak-buffer)))))

(cl-loop
 for f in
 '(nov-scroll-up  nov-scroll-down)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the next screenful."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'scroll)
       (dtk-speak (emacspeak-get-window-contents))))))

;;}}}
;;{{{Mode Hook:

(defun emacspeak-nov-mode-hook ()
  "Load directory-specific speech settings."
  (cl-declare (special emacspeak-speak-directory-settings))
  (let((locals (locate-dominating-file default-directory emacspeak-speak-directory-settings)))
       (when locals
         (load (expand-file-name emacspeak-speak-directory-settings locals)))))

(add-hook 'nov-mode-hook #'emacspeak-nov-mode-hook)
;;}}}
(provide 'emacspeak-nov)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
