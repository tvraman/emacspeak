;;; emacspeak-ocr.el --- ocr Front-end for emacspeak desktop
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for OCR
;;; Keywords: Emacspeak, ocr
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

;;; Copyright (C) 1999 T. V. Raman <raman@cs.cornell.edu>
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

;;{{{ required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)
(require 'emacspeak-table)
(require 'emacspeak-table-ui)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module defines Emacspeak's front-end to OCR.
;;; This module assumes that sane is installed and working
;;; for image acquisition,
;;; and that there is an OCR engine that can take acquired
;;; images and produce text.
;;;

;;}}}
;;{{{  variables

(defgroup emacspeak-ocr  nil
  "OCR front-end for emacspeak."
  :group 'emacspeak
  :prefix 'emacspeak-ocr)
(defcustom emacspeak-ocr-scan-image "scanimage"
  "Name of image acquisition program."
  :type 'string 
:group 'emacspeak-ocr)

(defcustom emacspeak-ocr-scan-image-options nil
  "Command line options to pass to image acquisition program."
:type '(repeat (string :tag "scanner options"))
:group 'emacspeak-ocr)

(defcustom emacspeak-ocr-engine nil
  "OCR engine to process acquired image."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-engine-options nil
  "Command line options to pass to OCR engine."
:type '(repeat (string :tag "OCR options"))
:group 'emacspeak-ocr)

;;}}}
;;{{{  helpers

;;}}}
;;{{{ interactive commands
(defun emacspeak-ocr ()
  "OCR front-end for Emacspeak desktop.
Image is acquired using user space tools from the SANE
package.
The acquired image is run through the OCR engine if one is
available,
and the results placed in a buffer that is suitable for
browsing the results."
  (interactive)
(emacspeak-auditory-icon 'task-done))

;;}}}
;;{{{  keymaps 

;;}}}
(provide 'emacspeak-ocr)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
