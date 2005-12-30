;;; emacspeak-add-log.el --- Speech-enable add-log
;;; $Id$
;;; $Author$
;;; Description:  speech-enable change-log-mode
;;; Keywords: Emacspeak,  Audio Desktop ChangeLogs
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
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction
;;; Commentary:

;;; speech-enables change-log-mode 
;;;Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'browse-url)
(require 'emacspeak-preamble)
(eval-when-compile
  (condition-case nil
      (require 'emacspeak-w3)
    (error nil)))

;;}}}
;;{{{ define personalities

(defgroup emacspeak-add-log nil
  "Customize Emacspeak for change-log-mode and friends."
  :group 'emacspeak)

(def-voice-font emacspeak-change-log-acknowledgement-personality
  voice-smoothen
  'change-log-acknowledgement-face
  "Personality used for acknowledgements."
  :group 'emacspeak-add-log)
(def-voice-font emacspeak-change-log-conditionals-personality 
  voice-animate
  'change-log-conditionals-face
  "Personality used for conditionals."
  :group 'emacspeak-add-log)

(def-voice-font emacspeak-change-log-date-personality
  voice-brighten
  'change-log-date-face
  "Personality used for dates."
  :group 'emacspeak-add-log)

(def-voice-font emacspeak-change-log-email-personality
  voice-womanize-1
  'change-log-email-face
  "Personality used for email address."
  :group 'emacspeak-add-log)

(def-voice-font emacspeak-change-log-file-personality
  voice-bolden
  'change-log-file-face
  "Personality used for file names."
  :group 'emacspeak-add-log)

(def-voice-font emacspeak-change-log-function-personality
  voice-bolden-extra
  'change-log-function-face
  "Personality used for function names."
  :group 'emacspeak-add-log)

(def-voice-font emacspeak-change-log-list-personality
  voice-lighten
  'change-log-list-face
  "Personality used for lists."
  :group 'emacspeak-add-log)

(def-voice-font emacspeak-change-log-name-personality
  voice-lighten-extra
  'change-log-name-face
  "Personality used for names."
  :group 'emacspeak-add-log)

;;}}}

(provide 'emacspeak-add-log)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
