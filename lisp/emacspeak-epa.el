;;; emacspeak-epa.el --- Speech-enable EasyPG Assistant
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable EPA An Emacs Interface to epa
;;; Keywords: Emacspeak,  Audio Desktop epa
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
;;; MERCHANTABILITY or FITNEPA FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; EPA == EasyPG Assistant
;;; Integrate GPG functionality into Emacs.
;;; Speech-enable all interactive commands.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Map Faces:
(voice-setup-add-map
 '(
   (epa-validity-high voice-animate)
   (epa-validity-medium voice-smoothen)
   (epa-validity-low voice-smoothen-extra)
   (epa-validity-disabled voice-monotone)
   (epa-string voice-lighten)
   (epa-mark voice-bolden)
   (epa-field-name voice-smoothen)
   (epa-field-body voice-animate)))

;;}}}
;;{{{ Advice Interactive Commands:

;; ("epa-decrypt-armor-in-region"
;; "epa-decrypt-file"
;; "epa-decrypt-region"
;; "epa-delete-keys"
;; "epa-dired-do-decrypt"
;; "epa-dired-do-encrypt"
;; "epa-dired-do-sign"
;; "epa-dired-do-verify"
;; "epa-encrypt-file"
;; "epa-encrypt-region"
;; "epa-exit-buffer"
;; "epa-export-keys"
;; "epa-file-disable"
;; "epa-file-enable"
;; "epa-file-name-regexp-update"
;; "epa-file-select-keys"
;; "epa-global-mail-mode"
;; "epa-import-armor-in-region"
;; "epa-import-keys"
;; "epa-import-keys-region"
;; "epa-info-mode"
;; "epa-insert-keys"
;; "epa-key-list-mode"
;; "epa-key-mode"
;; "epa-list-keys"
;; "epa-list-secret-keys"
;; "epa-mail-decrypt"
;; "epa-mail-encrypt"
;; "epa-mail-import-keys"
;; "epa-mail-mode"
;; "epa-mail-sign"
;; "epa-mail-verify"
;; "epa-mark-key"
;; "epa-sign-file"
;; "epa-sign-region"
;; "epa-unmark-key"
;; "epa-verify-cleartext-in-region"
;; "epa-verify-file"
;; "epa-verify-region")

;;}}}
(provide 'emacspeak-epa)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
