;;; emacspeak-cedet.el --- Speech enable CEDET
;;; $Id$
;;; $Author$ 
;;; Description: Auditory interface to CEDET
;;; Keywords: Emacspeak, Speak, Spoken Output, CEDET
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

;;; Copyright (c) 1995 -- 2004, T. V. Raman
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{  Introduction

;;; CEDET consists of speedbar semantic ede and friends.
;;; This module speech enables new functionality in semantic,
;;senator and friends

;;}}}
;;{{{ advice semantic completion

(loop for f in
      '(semantic-complete-complete-tab
        semantic-complete-complete-space)
      do
      (eval 
       `(defadvice ,f (around emacspeak pre
			      act comp)
	  "Provide auditory feedback."
	  (let ((prior (point ))
		(dtk-stop-immediately t))
	    (emacspeak-kill-buffer-carefully "*Completions*")
	    ad-do-it
	    (let ((completions-buffer (get-buffer "*Completions*")))
	      (if (> (point) prior)
		  (tts-with-punctuations "all"
					 (dtk-speak (buffer-substring prior (point ))))
		(when (and completions-buffer
			   (window-live-p (get-buffer-window completions-buffer )))
		  (save-excursion
		    (set-buffer completions-buffer )
		    (emacspeak-prepare-completions-buffer)
		    (tts-with-punctuations "all"
					   (dtk-speak (buffer-string )))))))
	    ad-return-value))))

;;}}}
(provide 'emacspeak-cedet )
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
