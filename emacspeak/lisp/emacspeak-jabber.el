;;; emacspeak-jabber.el --- Speech-Enable jabber 
;;; $Id$
;;; $Author$
;;; Description: speech-enable jabber 
;;; Keywords: Emacspeak, jabber
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

;;{{{ Introduction:

;;; Commentary:
;;; emacs-jabber.el implements a  jabber client for emacs
;;; emacs-jabber is hosted at sourceforge.
;;; I use emacs-jabber with my gmail.com account

;;; Code:

;;}}}
;;{{{  Required modules

(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice interactive commands:

;;}}}
;;{{{ silence keepalive

(loop for f in 
'(jabber-keepalive-do
  jabber-keepalive-got-response)
do
(eval
`(defadvice ,f (around emacspeak pre act comp)
   "Silence keepalive messages."
   (let ((emacspeak-speak-messages nil))
     ad-do-it
ad-return-value))))

;;}}}
;;{{{ chat buffer:

(defadvice jabber-chat-buffer-send (after emacspeak pre act comp)
  "Produce auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))
(loop for f in 
'(jabber-chat-with
  jabber-chat-with-jid-at-point)
do
(eval
`(defadvice ,f (after emacspeak pre act comp)
   "Silence keepalive messages."
   (when (interactive-p)
     (emacspeak-auditory-icon 'open-object)
     (emacspeak-speak-mode-line)))))
;;}}}
;;{{{ alerts
(defcustom emacspeak-jabber-speak-presence-alerts nil
  "Set to T if you want to hear presence alerts."
:type  'boolean
:group 'emacspeak-jabber)

(defadvice jabber-presence-default-message (around emacspeak pre
                                                   act comp)
  "Allow emacspeak to control if the message is spoken."
  (cond
   ((emacspeak-jabber-speak-presence-alerts ad-do-it))
   (t (let ((emacspeak-speak-messages nil))
        ad-do-i)))
  ad-return-value)

;;{{{ interactive commands:

(defun emacspeak-jabber-popup-roster ()
  "Pop to Jabber roster."
  (interactive)
  (declare (special jabber-roster-buffer))
  (pop-to-buffer jabber-roster-buffer)
  (goto-char (point-min))
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

;;}}}

;;}}}
(provide 'emacspeak-jabber)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
