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
(require 'derived)

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

(defcustom emacspeak-ocr-image-compress "tiffcp"
  "Command used to compress the scanned tiff file."
  :type 'string
  :group 'emacspeak-ocr)
(defcustom emacspeak-ocr-image-compress-options  
  "-c -g3 "
  "Options used for compressing tiff image."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-engine nil
  "OCR engine to process acquired image."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-engine-options nil
  "Command line options to pass to OCR engine."
  :type'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-working-directory
  (expand-file-name "~/ocr")
  "Directory where images and OCR results
will be placed."
  :group 'emacspeak-ocr
  :type 'string)

;;}}}
;;{{{  helpers

(defvar emacspeak-ocr-buffer-name "*ocr*"
  "Name of OCR working buffer.")

(defsubst emacspeak-ocr-get-buffer ()
  "Return OCR working buffer."
  (declare (special emacspeak-ocr-buffer-name))
  (get-buffer-create emacspeak-ocr-buffer-name))

(defsubst emacspeak-ocr-get-image-name ()
  "Return name of current image."
  (declare (special emacspeak-ocr-document-name))
  (format "%s.tiff" emacspeak-ocr-document-name))

;;}}}
;;{{{  emacspeak-ocr mode

(define-derived-mode emacspeak-ocr-mode fundamental-mode 
  "Major mode for document scanning and  OCR."
  "Major mode for document scanning and OCR\n]\n
\\{emacspeak-ocr-mode-map}")

(define-key emacspeak-ocr-mode-map "i"
  'emacspeak-ocr-scan-image)
(define-key emacspeak-ocr-mode-map "o" 'emacspeak-ocr-recognize-image)
(define-key emacspeak-ocr-mode-map "n"
  'emacspeak-ocr-name-document)
(define-key emacspeak-ocr-mode-map "a" 'emacspeak-ocr-append-page)
(define-key emacspeak-ocr-mode-map "d" 'dired)
(define-key emacspeak-ocr-mode-map "[" 'backward-page)
(define-key emacspeak-ocr-mode-map "]"'forward-page)

;;}}}
;;{{{ interactive commands

(defun emacspeak-ocr ()
  "OCR front-end for Emacspeak desktop.  Image is acquired
using user space tools from the SANE package.  The acquired
image is run through the OCR engine if one is available, and
the results placed in a buffer that is suitable for browsing
the results."
  (interactive)
  (declare (special emacspeak-ocr-working-directory
                    buffer-read-only))
  (let  ((buffer (emacspeak-ocr-get-buffer )))
    (save-excursion
      (set-buffer buffer)
      (emacspeak-ocr-mode)
      (when (file-exists-p emacspeak-ocr-working-directory)
        (cd emacspeak-ocr-working-directory))
      (switch-to-buffer buffer)
      (setq buffer-read-only t)
      (emacspeak-auditory-icon 'open-object)
      (message "Hit return to start scanning."))))

;;; naming a document 

(defcustom emacspeak-ocr-document-name nil
  "Names document being scanned.
This name will be used as the prefix for naming image and
text files produced in this scan."
  :type 'string 
  :group 'emacspeak-ocr)

(make-variable-buffer-local 'emacspeak-ocr-document-name)

(defun emacspeak-ocr-name-document (name)
  "Name document being scanned in the current OCR buffer.
Pick a short but meaningful name."
  (interactive
   (list
    (read-from-minibuffer "Document name: ")))
  (declare (special emacspeak-ocr-document-name))
  (setq emacspeak-ocr-document-name name)
  (rename-buffer
   (format "*%s-ocr*" name)
   'unique)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))

(defun emacspeak-ocr-scan-image ()
  "Acquire page image."
  (interactive)
  (declare (special emacspeak-ocr-scan-image
                    emacspeak-ocr-scan-image-options
                    emacspeak-ocr-compress-image
                    emacspeak-ocr-compress-image-options
                    emacspeak-ocr-document-name))
  (let ((image-name (emacspeak-ocr-get-image-name)))
    (shell-command
     (concat
      (format "%s %s > temp.tiff;\n"
              emacspeak-ocr-scan-image
              emacspeak-ocr-scan-image-options )
      (format "%s %s  temp.tiff %s;\n"
              emacspeak-ocr-compress-image
              emacspeak-ocr-compress-image-options 
              image-name)
      (format "rm -f temp.tiff")))
    (message "Acquired  image to file %s"
             image-name)))

(defun emacspeak-ocr-recognize-image ()
  "Run OCR engine on current image."
  (interactive)
  (declare (special emacspeak-ocr-engine
                    emacspeak-ocr-engine-options
                    emacspeak-ocr-process))
  (setq emacspeak-ocr-process
        (start-process 
         "ocr"
         (current-buffer)
         emacspeak-ocr-engine
         emacspeak-ocr-engine-options
         (emacspeak-ocr-get-image-name)))
  (set-process-sentinel emacspeak-ocr-process
                        'emacspeak-ocr-process-sentinel)
  (message "Launched OCR engine."))
         
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
