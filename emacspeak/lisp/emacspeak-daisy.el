;;; emacspeak-daisy.el --- daisy Front-end for emacspeak desktop
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak front-end for DAISY Talking Books
;;; Keywords: Emacspeak, daisy Digital Talking Books
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

;;; Daisy Digital Talking Book Reader

;;}}}
;;{{{ required modules

;;; Code:

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'custom)
(require 'emacspeak-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)
(require 'derived)
(require 'xml-parse)
(eval-when-compile (require 'emacspeak-keymap))

;;}}}
;;{{{  Customization variables

(defgroup emacspeak-daisy nil
  "Daisy Digital Talking Books  for the Emacspeak desktop."
  :group 'emacspeak)

;;}}}
;;{{{  helpers
(defvar emacspeak-daisy-frame-seconds 0.02612219949104336
  "Number of seconds in a frame.")

(defsubst emacspeak-daisy-time-string-to-frame (timestr)
  "Convert time string (hh:mm:ss.SS) to frame number.(0.026s/f)"
  (declare (special emacspeak-daisy-frame-seconds))
  (save-match-data
    (string-match "\\(..\\):\\(..\\):\\(...**\\)" timestr)
    (let*
        ((h (string-to-number (substring timestr (match-beginning 1) (match-end 1))))
         (m (string-to-number (substring timestr (match-beginning 2) (match-end 2))))
         (s (string-to-number (substring timestr (match-beginning 3) (match-end 3))))
         (total (+  (* 3600 h)
                    (* m 60)
                    s)))
       (round
        (/ total   emacspeak-daisy-frame-seconds)))))





(defvar emacspeak-daisy-base-uri nil
  "Base URI of current book.
Used to resolve relative URIs.")


(defsubst emacspeak-daisy-resolve-uri (relative)
  "Resolve relative URI with respect to emacspeak-daisy-base-uri."
(declare (special emacspeak-daisy-base-uri))
(expand-file-name relative emacspeak-daisy-base-uri))





;;}}}
;;{{{  play a clip 

(defvar emacspeak-daisy-mpg123-player "mpg123"
  "MPG123 executable for playing mp3 files.")

(defun emacspeak-daisy-play-audio-clip (clip)
  "Play clip specified by clip.
Clip is the result of parsing element <audio .../> as defined by Daisy 3."
  (declare (special emacspeak-daisy-mpg123-player))
  (unless
      (and (listp clip)
 (string-equal "audio" (caar clip)))
    (error "Invalid audio clip."))
  (let ((src (xml-tag-attr  clip "src"))
        (begin (xml-tag-attr  clip "clipBegin"))
        (end (xml-tag-attr  clip "clipEnd"))
        (first nil)
        (last nil)
        (path nil))
    (setq path (emacspeak-daisy-resolve-uri src))
    (setq first (emacspeak-daisy-time-string-to-frame begin))
    (setq last (emacspeak-daisy-time-string-to-frame end))
    (start-process "mpg123"  nil 
                   emacspeak-daisy-mpg123-player
                   "-k"
                   (format "%s"  (1- first))
                   "-n"
                   (format "%s"  (- last first ))
                   path)))

;;}}}
;;{{{  table of handlers 

(defvar emacspeak-daisy-handler-table (make-hash-table :test #'string-equal)
  "Table that maps elements to handlers.")

(defsubst emacspeak-daisy-get-handler (element )
  "Get handler for element."
  (declare (special emacspeak-daisy-handler-table))
  (gethash element emacspeak-daisy-handler-table))

(defsubst emacspeak-daisy-set-handler (element handler)
  "Set handler for element."
  (declare (special emacspeak-daisy-handler-table))
  (setf (gethash element emacspeak-daisy-handler-table) handler))

;;}}}
;;{{{ Install handlers 
;;; elements
(defvar emacspeak-daisy-xml-elements 
  (list
   "ncx "
   "head "
   "title"
   "doctitle"
   "text"
   "audio"
   "content"
   "navStruct"
   "navObject")
  "Daisy XML elements.")

(loop for e in emacspeak-daisy-xml-elements
      do
      (emacspeak-daisy-set-handler e
                                   (intern
                                    (format
                                     "emacspeak-daisy-%s-handler" e))))
;;}}}
;;{{{ Define handlers 


(defsubst emacspeak-daisy-apply-handler (element)
  "Lookup and apply installed handler."
  (let* ((tag (xml-tag-name element))
         (handler  (emacspeak-daisy-get-handler tag)))
    (cond
     ((and handler
           (fboundp handler))(funcall handler element))
     (t
      (insert
       (format "Handler for %s not implemented yet.\n" tag))))))

(defun  emacspeak-daisy-ncx-handler (ncx)
  "Process top-level NCX element."
  (let ((buffer (get-buffer-create "*daisy*")))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (mapc 'emacspeak-daisy-apply-handler (xml-tag-children ncx))
      (emacspeak-daisy-mode))
(switch-to-buffer buffer)))

(defun emacspeak-daisy-text-handler (element)
  "Handle element <text>...</text>."
  (mapc #'insert (xml-tag-children element))
  (insert "\n"))
   

(defun emacspeak-daisy-doctitle-handler (element)
  "Handle <doctitle>...</doctitle>"
  (let ((text (xml-tag-child  element "text"))
        (audio (xml-tag-child element "audio"))
        (start (point)))
    (emacspeak-daisy-text-handler   text)
    (put-text-property start (point)
                       'audio audio)))
  


;;}}}
;;{{{  emacspeak-daisy mode

(declaim (special emacspeak-daisy-mode-map))

(define-derived-mode emacspeak-daisy-mode text-mode 
  "Major mode for Daisy Digital Talking Books.\n"
  " An DAISY front-end for the Emacspeak desktop.

Pre-requisites:

0) mpg123 for playing mp3 files
1) libxml and libxslt packages 
2) xml-parse.el for parsing XML in Emacs Lisp.


The Emacspeak DAISY front-end is launched by command
emacspeak-daisy bound to \\[emacspeak-daisy].  

This command switches to a special buffer that has DAISY
commands bounds to single keystrokes-- see the ke-binding
list at the end of this description.  Use Emacs online help
facility to look up help on these commands.

emacspeak-daisy-mode provides the necessary functionality to
navigate and listen to Daisy talking books. 

Here is a list of all emacspeak DAISY commands along with their key-bindings:

\\{emacspeak-daisy-mode-map}"
  (progn
    (emacspeak-keymap-remove-emacspeak-edit-commands emacspeak-daisy-mode-map)))

(define-key emacspeak-daisy-mode-map "?" 'describe-mode)

(define-key emacspeak-daisy-mode-map "\C-m" 'emacspeak-daisy-play-audio-clip-under-point)

;;}}}
;;{{{ interactive commands

(defun emacspeak-daisy-play-audio-clip-under-point ()
  "Play audio clip under point."
  (interactive)
  (let ((clip (get-text-property (point) 'audio)))
    (cond
     (clip
  (emacspeak-daisy-play-audio-clip clip))
     (t (error "No audio clip under point.")))))

;;}}}
(provide 'emacspeak-daisy)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
