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

;;{{{  Introduction:

;;; Commentary:

;;; This module defines Emacspeak front-end to OCR.
;;; This module assumes that sane is installed and working
;;; for image acquisition,
;;; and that there is an OCR engine that can take acquired
;;; images and produce text.
;;; Prerequisites:
;;; Sane installed and working.
;;; scanimage to generate tiff files from scanner.
;;; tiffcp to compress the tiff file.
;;; working ocr executable 
;;; by default this module assumes that the OCR executable
;;; is named "ocr"

;;; Code:

;;}}}
;;{{{ required modules
(require 'emacspeak-preamble)
;;}}}
;;{{{  Customization variables
;;;###autoload
(defgroup emacspeak-ocr nil
  "Emacspeak front end for scanning and OCR.
Pre-requisites:
SANE for image acquisition.
OCR engine for optical character recognition."
  :group 'emacspeak
  :prefix "emacspeak-ocr-")

(defcustom emacspeak-ocr-scan-image "scanimage"
  "Name of image acquisition program."
  :type 'string 
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-scan-image-options 
  "--format tiff --resolution 400"
  "Command line options to pass to image acquisition program."
  :type 'string 
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-compress-image "tiffcp"
  "Command used to compress the scanned tiff file."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-image-extension ".tiff"
  "Filename extension used for acquired image."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-compress-image-options   
  "-c g3 "
  "Options used for compressing tiff image."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-engine "ocr"
  "OCR engine to process acquired image."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-engine-options nil
  "Command line options to pass to OCR engine."
  :type'(repeat
         (string :tag "Option"))
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-working-directory
  (expand-file-name "ocr/"
                    emacspeak-resource-directory)
  "Directory where images and OCR results
will be placed."
  :group 'emacspeak-ocr
  :type 'string)

(defcustom emacspeak-ocr-scan-photo-options 
  "--mode color --format=pnm"
  "Options  used when scanning in photographs."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-photo-compress "cjpeg"
  "Program to create JPEG compressed images."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-compress-photo-options
  "-optimize -progressive"
  "Options used when created JPEG from  scanned photographs."
  :type 'string
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-keep-uncompressed-image nil
  "If set to T, uncompressed image is not removed."
  :type 'boolean
  :group 'emacspeak-ocr)

(defcustom emacspeak-ocr-jpeg-metadata-writer "wrjpgcom"
  "Program to add metadata to JPEG files."
  :type 'string
  :group 'emacspeak-ocr)

;;}}}
;;{{{  helpers

(defvar emacspeak-ocr-current-page-number  nil
  "Number of current page in document.")

(make-variable-buffer-local
 'emacspeak-ocr-current-page-number)

(defvar emacspeak-ocr-last-page-number nil
  "Number of last page in document.")

(make-variable-buffer-local 'emacspeak-ocr-last-page-number)

(defvar emacspeak-ocr-page-positions nil
  "Vector holding page start positions.")

(make-variable-buffer-local 'emacspeak-ocr-page-positions)

(defvar emacspeak-ocr-buffer-name "*ocr*"
  "Name of OCR working buffer.")

(defsubst emacspeak-ocr-get-buffer ()
  "Return OCR working buffer."
  (get-buffer-create
   (format  "*%s-ocr*"
	    (emacspeak-ocr-default-name))))

(defsubst emacspeak-ocr-get-text-name ()
  "Return name of current text document."
  (declare (special emacspeak-ocr-document-name))
  (format "%s.text" emacspeak-ocr-document-name))

(defsubst emacspeak-ocr-get-image-name (extension)
  "Return name of current image."
  (declare (special emacspeak-ocr-document-name
                    emacspeak-ocr-last-page-number))
  (format "%s-p%s%s"
          emacspeak-ocr-document-name
          (1+ emacspeak-ocr-last-page-number)
	  extension))

(defsubst emacspeak-ocr-get-page-name ()
  "Return name of current page."
  (declare (special emacspeak-ocr-document-name
                    emacspeak-ocr-current-page-number))
  (format "%s-p%s.txt"
          emacspeak-ocr-document-name
          emacspeak-ocr-current-page-number))

(defvar emacspeak-ocr-mode-line-format
  '(
    (buffer-name)
    " "
    "page-"
    emacspeak-ocr-current-page-number
    "/"
    emacspeak-ocr-last-page-number)
  "Mode line format for OCR buffer.")

(defsubst emacspeak-ocr-get-mode-line-format ()
  "Return string suitable for use as the mode line."
  (declare (special major-mode
                    emacspeak-ocr-current-page-number))
  (format "%s Page-%s/%s %s"
          (buffer-name)
          emacspeak-ocr-current-page-number
          emacspeak-ocr-last-page-number
          major-mode))

(defsubst emacspeak-ocr-update-mode-line()
  "Update mode line for OCR mode."
  (declare (special mode-line-format))
  (setq mode-line-format
        (emacspeak-ocr-get-mode-line-format)))

;;}}}
;;{{{  emacspeak-ocr mode
(declaim (special emacspeak-ocr-mode-map))

(define-derived-mode emacspeak-ocr-mode text-mode 
  "Major mode for document scanning and  OCR.\n"
  " An OCR front-end for the Emacspeak desktop.

Pre-requisites:

1) A working scanner back-end like SANE on Linux.

2) An OCR engine.

1: Make sure your scanner back-end works, and that you have
the utilities to scan a document and acquire an image as a
tiff file.  Then set variable
emacspeak-ocr-scan-image-program to point at this utility.
By default, this is set to `scanimage' which is the image
scanning utility provided by SANE.

By default, this front-end attempts to compress the acquired
tiff image; make sure you have a utility like tiffcp.
Variable emacspeak-ocr-compress-image is set to `tiffcp' by
default; if you use something else, you should customize
this variable.

2: Next, make sure you have an OCR engine installed and
working.  By default this front-end assumes that OCR is
available as /usr/bin/ocr.

Once you have ensured that acquiring an image and applying
OCR to it work independently of Emacs, you can use this
Emacspeak front-end to enable easy OCR access from within
Emacspeak.

The Emacspeak OCR front-end is launched by command
emacspeak-ocr bound to \\[emacspeak-ocr].  

This command switches to a special buffer that has OCR
commands bounds to single keystrokes-- see the ke-binding
list at the end of this description.  Use Emacs online help
facility to look up help on these commands.

emacspeak-ocr-mode provides the necessary functionality to
scan, OCR, read and save documents.  By default, scanned
images and the resulting text are saved under directory
~/ocr; see variable emacspeak-ocr-working-directory.
Invoking command emacspeak-ocr-open-working-directory bound
to \\[emacspeak-ocr-open-working-directory] will open this directory.

By default, the document being scanned is named `untitled'.
You can name the document by using command
emacspeak-ocr-name-document bound to
\\[emacspeak-ocr-name-document].  The document name is used
in constructing the name of the image and text files.

Key Bindings: 

See \\{emacspeak-ocr-mode-map}.
"
  (progn
    (setq emacspeak-ocr-current-page-number 0
          emacspeak-ocr-last-page-number 0
          emacspeak-ocr-page-positions
          (make-vector 25 nil))
    (emacspeak-ocr-update-mode-line)
    (emacspeak-keymap-remove-emacspeak-edit-commands
     emacspeak-ocr-mode-map)))

(define-key emacspeak-ocr-mode-map "?" 'describe-mode)
(define-key emacspeak-ocr-mode-map "c" 'emacspeak-ocr-customize)
(define-key emacspeak-ocr-mode-map "q" 'bury-buffer)
(define-key emacspeak-ocr-mode-map "w" 'emacspeak-ocr-write-document)
(define-key emacspeak-ocr-mode-map "\C-m"  'emacspeak-ocr-scan-and-recognize)
(define-key emacspeak-ocr-mode-map "i" 'emacspeak-ocr-scan-image)
(define-key emacspeak-ocr-mode-map "j" 'emacspeak-ocr-scan-photo)
(define-key emacspeak-ocr-mode-map "o" 'emacspeak-ocr-recognize-image)
(define-key emacspeak-ocr-mode-map "n" 'emacspeak-ocr-name-document)
(define-key emacspeak-ocr-mode-map "d" 'emacspeak-ocr-open-working-directory)
(define-key emacspeak-ocr-mode-map "[" 'emacspeak-ocr-backward-page)
(define-key emacspeak-ocr-mode-map "]"'emacspeak-ocr-forward-page)
(define-key emacspeak-ocr-mode-map "p" 'emacspeak-ocr-page)
(define-key emacspeak-ocr-mode-map "s" 'emacspeak-ocr-save-current-page)
(define-key emacspeak-ocr-mode-map " "
  'emacspeak-ocr-read-current-page)
(define-key emacspeak-ocr-mode-map "I"
  'emacspeak-ocr-set-scan-image-options)
(define-key emacspeak-ocr-mode-map "C" 'emacspeak-ocr-set-compress-image-options)
(loop for i from 1 to 9
      do
      (define-key emacspeak-ocr-mode-map
        (format "%s" i)
        'emacspeak-ocr-page))

;;}}}
;;{{{ interactive commands
(defun emacspeak-ocr-customize ()
  "Customize OCR settings."
  (interactive)
  (customize-group 'emacspeak-ocr)
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))

(defun emacspeak-ocr-default-name ()
  "Return a default name for OCR document."
  (format-time-string "%m-%d-%y"))

;;;###autoload
(defun emacspeak-ocr ()
  "An OCR front-end for the Emacspeak desktop.  

Page image is acquired using tools from the SANE package.
The acquired image is run through the OCR engine if one is
available, and the results placed in a buffer that is
suitable for browsing the results.

For detailed help, invoke command emacspeak-ocr bound to
\\[emacspeak-ocr] to launch emacspeak-ocr-mode, and press
`?' to display mode-specific help for emacspeak-ocr-mode."
  (interactive)
  (declare (special emacspeak-ocr-working-directory
                    emacspeak-ocr-document-name
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
      (setq emacspeak-ocr-document-name (emacspeak-ocr-default-name))
      (emacspeak-speak-mode-line))))

(defvar emacspeak-ocr-document-name nil
  "Names document being scanned.
This name will be used as the prefix for naming image and
text files produced in this scan.")

(make-variable-buffer-local 'emacspeak-ocr-document-name)
(defun emacspeak-ocr-name-document (name)
  "Name document being scanned in the current OCR buffer.
Pick a short but meaningful name."
  (interactive
   (list
    (read-from-minibuffer "Document name: ")))
  (declare (special emacspeak-ocr-document-name
                    mode-line-format))
  (setq emacspeak-ocr-document-name name)
  (rename-buffer
   (format "*%s-ocr*" name)
   'unique)
  (emacspeak-ocr-update-mode-line)
  (emacspeak-auditory-icon 'select-object)
  (emacspeak-speak-mode-line))
(defun emacspeak-ocr-scan-image ()
  "Acquire page image."
  (interactive)
  (declare (special emacspeak-speak-messages
                    emacspeak-ocr-last-page-number
                    emacspeak-ocr-image-extension
                    emacspeak-ocr-keep-uncompressed-image
                    emacspeak-ocr-scan-image
                    emacspeak-ocr-scan-image-options
                    emacspeak-ocr-compress-image
                    emacspeak-ocr-compress-image-options
                    emacspeak-ocr-document-name))
  (let ((image-name (emacspeak-ocr-get-image-name
                     emacspeak-ocr-image-extension)))
    (let ((emacspeak-speak-messages nil))
      (shell-command
       (concat
        (format "%s %s > temp%s;\n"
                emacspeak-ocr-scan-image
                emacspeak-ocr-scan-image-options 
                emacspeak-ocr-image-extension)
        (format "%s %s  temp%s %s ;\n"
                emacspeak-ocr-compress-image
                emacspeak-ocr-compress-image-options
                emacspeak-ocr-image-extension
                image-name)
        (if emacspeak-ocr-keep-uncompressed-image
            (format "echo \'Uncompressed image not removed.'")
          (format "rm -f temp%s"
                  emacspeak-ocr-image-extension)))))
    (when (interactive-p)
      (setq emacspeak-ocr-last-page-number
	    (1+ emacspeak-ocr-last-page-number)))
    (message "Acquired  image to file %s"
             image-name)))

(defun emacspeak-ocr-scan-photo (&optional metadata)
  "Scan in a photograph.
The scanned image is converted to JPEG."
  (interactive "P")
  (declare (special emacspeak-speak-messages
                    emacspeak-ocr-jpeg-metadata-writer
                    emacspeak-ocr-photo-compress-options
                    emacspeak-ocr-scan-photo-options
                    emacspeak-ocr-keep-uncompressed-image
                    emacspeak-ocr-scan-image
                    emacspeak-ocr-compress-photo
                    emacspeak-ocr-image-extension
                    emacspeak-ocr-document-name))
  (let (
        (jpg (emacspeak-ocr-get-image-name ".jpg"))
        (pnm (emacspeak-ocr-get-image-name ".pnm")))
    (shell-command
     (concat
      (format "%s %s > temp.pnm;\n"
              emacspeak-ocr-scan-image
              emacspeak-ocr-scan-photo-options)
      (format "%s %s  temp.pnm > %s ;\n"
              emacspeak-ocr-compress-photo
              emacspeak-ocr-compress-photo-options
              jpg)
      (if emacspeak-ocr-keep-uncompressed-image
          (format "mv temp.pnm %s"
                  pnm)
        (format "rm -f temp.pnm"))))
    (when (and metadata
               (interactive-p))
      (setq metadata
            (read-from-minibuffer "Enter picture description: "))
      (let ((tempfile (format "temp%s.jpg" (gensym))))
        (shell-command
         (format  "mv %s %s; wrjpgcom -c '%s' %s > %s; rm -f %s"
                  jpg  tempfile
                  emacspeak-ocr-jpeg-metadata-writer metadata 
                  tempfile jpg
                  tempfile))))
    (message "Acquired  image to file %s" jpg)
    (setq emacspeak-ocr-last-page-number
          (1+ emacspeak-ocr-last-page-number))))

(defvar emacspeak-ocr-process nil
  "Handle to OCR process.")
(defun emacspeak-ocr-write-document ()
  "Writes out recognized text from all pages in current document."
  (interactive)
  (cond
   ((= 0 emacspeak-ocr-current-page-number)
    (message "No pages in current document."))
   (t (write-region
       (point-min)
       (point-max)
       (emacspeak-ocr-get-text-name))
      (emacspeak-auditory-icon 'save-object))))
(defun emacspeak-ocr-save-current-page ()
  "Writes out recognized text from current page
to an appropriately named file."
  (interactive)
  (declare (special emacspeak-ocr-current-page-number
                    emacspeak-ocr-page-positions))
  (cond
   ((= 0 emacspeak-ocr-current-page-number)
    (message "No pages in current document."))
   (t (write-region
       (aref emacspeak-ocr-page-positions
             emacspeak-ocr-current-page-number)
       (if (= emacspeak-ocr-current-page-number
              emacspeak-ocr-last-page-number)
           (point-max)
         (aref emacspeak-ocr-page-positions (1+
                                             emacspeak-ocr-current-page-number)))
       (emacspeak-ocr-get-page-name))
      (emacspeak-auditory-icon 'save-object))))

(defun emacspeak-ocr-process-sentinel  (process state)
  "Alert user when OCR is complete."
  (declare (special emacspeak-ocr-page-positions
                    emacspeak-ocr-last-page-number
                    emacspeak-ocr-current-page-number))
  (setq emacspeak-ocr-current-page-number
        emacspeak-ocr-last-page-number)
  (emacspeak-auditory-icon 'task-done)
  (goto-char (aref emacspeak-ocr-page-positions
                   emacspeak-ocr-current-page-number))
  (emacspeak-ocr-save-current-page)
  (emacspeak-ocr-update-mode-line)
  (emacspeak-speak-line))
(defun emacspeak-ocr-recognize-image ()
  "Run OCR engine on current image.
Prompts for image file if file corresponding to the expected
`current page' is not found."
  (interactive)
  (declare (special emacspeak-ocr-engine
                    emacspeak-ocr-engine-options
                    emacspeak-ocr-process
                    emacspeak-ocr-last-page-number
                    emacspeak-ocr-page-positions
                    emacspeak-ocr-image-extension))
  (let ((inhibit-read-only t)
        (image-name
         (if (file-exists-p (emacspeak-ocr-get-image-name emacspeak-ocr-image-extension))
             (emacspeak-ocr-get-image-name emacspeak-ocr-image-extension)
           (expand-file-name 
            (read-file-name "Image file to recognize: ")))))
    (goto-char (point-max))
    (emacspeak-auditory-icon 'select-object)
    (setq emacspeak-ocr-last-page-number
          (1+ emacspeak-ocr-last-page-number))
    (aset emacspeak-ocr-page-positions
          emacspeak-ocr-last-page-number
          (+ 3 (point)))
    (insert
     (format "\n%c\nPage %s\n" 12
             emacspeak-ocr-last-page-number))
    (setq emacspeak-ocr-process
          (apply 'start-process 
		 "ocr"
		 (current-buffer)
		 emacspeak-ocr-engine
		 image-name
		 emacspeak-ocr-engine-options))
    (set-process-sentinel emacspeak-ocr-process
                          'emacspeak-ocr-process-sentinel)
    (message "Launched OCR engine.")))

(defun emacspeak-ocr-scan-and-recognize ()
  "Scan in a page and run OCR engine on it.
Use this command once you've verified that the separate
steps of acquiring an image and running the OCR engine work
correctly by themselves."
  (interactive)
  (emacspeak-ocr-scan-image)
  (emacspeak-ocr-recognize-image))

(defun emacspeak-ocr-open-working-directory ()
  "Launch dired on OCR working directory."
  (interactive)
  (declare (special emacspeak-ocr-working-directory))
  (switch-to-buffer
   (dired-noselect emacspeak-ocr-working-directory))
  (emacspeak-auditory-icon 'open-object)
  (emacspeak-speak-mode-line))
(defun emacspeak-ocr-forward-page (&optional count-ignored)
  "Like forward page, but tracks page number of current document."
  (interactive "p")
  (declare (special emacspeak-ocr-page-positions
                    emacspeak-ocr-last-page-number
                    emacspeak-ocr-current-page-number))
  (cond
   ((= 0 emacspeak-ocr-current-page-number)
    (message "No pages in current document."))
   ((= emacspeak-ocr-last-page-number
       emacspeak-ocr-current-page-number)
    (goto-char
     (point-max))
    (emacspeak-auditory-icon 'select-object)
    (message "This is the last page."))
   (t (setq emacspeak-ocr-current-page-number
            (1+ emacspeak-ocr-current-page-number))
      (goto-char (aref emacspeak-ocr-page-positions
                       emacspeak-ocr-current-page-number))
      (emacspeak-ocr-update-mode-line)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))))
(defun emacspeak-ocr-backward-page (&optional count-ignored)
  "Like backward page, but tracks page number of current document."
  (interactive "p")
  (declare (special emacspeak-ocr-page-positions
                    emacspeak-ocr-current-page-number))
  (cond
   ((= 0 emacspeak-ocr-current-page-number)
    (message "No pages in current document."))
   ((= 1
       emacspeak-ocr-current-page-number)
    (goto-char
     (aref emacspeak-ocr-page-positions
           emacspeak-ocr-current-page-number))
    (emacspeak-auditory-icon 'select-object)
    (message "This is the first page."))
   (t (setq emacspeak-ocr-current-page-number
            (1- emacspeak-ocr-current-page-number))
      (emacspeak-ocr-update-mode-line)
      (goto-char (aref emacspeak-ocr-page-positions
                       emacspeak-ocr-current-page-number))
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))))

(defsubst emacspeak-ocr-goto-page (page)
  "Move to specified page."
  (declare (special emacspeak-ocr-page-positions))
  (goto-char
   (aref emacspeak-ocr-page-positions page))
  (emacspeak-ocr-update-mode-line)
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-speak-line)
  )
(defun emacspeak-ocr-page ()
  "Move to specified page."
  (interactive )
  (when (= 0 emacspeak-ocr-last-page-number)
    (error "No pages in current document."))
  (let ((page
         (condition-case nil
             (read (format "%c" last-input-event ))
           (error nil ))))
    (or (numberp page)
        (setq page
              (read-minibuffer
               (format "Page number between 1 and %s: "
                       emacspeak-ocr-last-page-number))))
    (cond
     ((> page emacspeak-ocr-last-page-number)
      (message "Not that many pages in document."))
     (t 
      (emacspeak-ocr-goto-page page)))))
(defun emacspeak-ocr-read-current-page ()
  "Speaks current page."
  (interactive)
  (declare (special emacspeak-ocr-page-positions
                    emacspeak-ocr-current-page-number
                    emacspeak-ocr-last-page-number))
  (cond
   ((= emacspeak-ocr-current-page-number
       emacspeak-ocr-last-page-number)
    (emacspeak-speak-region
     (aref emacspeak-ocr-page-positions
           emacspeak-ocr-current-page-number)
     (point-max)))
   (t (emacspeak-speak-region
       (aref emacspeak-ocr-page-positions
             emacspeak-ocr-current-page-number)
       (aref emacspeak-ocr-page-positions
             (1+ emacspeak-ocr-current-page-number))))))

(defun emacspeak-ocr-set-scan-image-options  (setting)
  "Interactively update scan image options.
Prompts with current setting in the minibuffer.
Setting persists for current Emacs session."
  (interactive
   (list
    (read-from-minibuffer
     "Scan image settings:"
     emacspeak-ocr-scan-image-options)))
  (declare (special emacspeak-ocr-scan-image-options))
  (setq emacspeak-ocr-scan-image-options setting))

(defun emacspeak-ocr-set-compress-image-options  (setting)
  "Interactively update  image compression options.
Prompts with current setting in the minibuffer.
Setting persists for current Emacs session."
  (interactive
   (list
    (read-from-minibuffer
     "Image compression settings: "
     emacspeak-ocr-compress-image-options)))
  (declare (special emacspeak-ocr-compress-image-options))
  (setq emacspeak-ocr-compress-image-options setting))

;;}}}
(provide 'emacspeak-ocr)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
