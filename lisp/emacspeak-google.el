;;; emacspeak-google.el --- Google Search Tools
;;; $Id: emacspeak-google.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable GOOGLE An Emacs Interface to google
;;; Keywords: Emacspeak,  Audio Desktop google
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman
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
;;; MERCHANTABILITY or FITNGOOGLE FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; There are a number of search tools that can be implemented on
;;; the Google search page --- in a JS-powered browser, these
;;; show up as the Google tool-belt.
;;; This module implements a minor mode for use in Google result
;;; pages that enables these tools via single keyboard commands.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'easy-mmode)

;;}}}
;;{{{ Data Structures 

;;; One tool on a tool-belt

(defstruct emacspeak-google-tool
  name ; human readable
  param ; url param bit
  range ; range of possible values
  value ; current setting
  )

(defvar emacspeak-google-toolbelt nil
  "List of tools on the toolbelt.")

(make-variable-buffer-local 'emacspeak-google-toolbelt)

(defun emacspeak-google-toolbelt-to-tbs (toolbelt)
  "Return value for use in tbs parameter in search queries."
  (mapconcat
   #'(lambda (tool)
       (format "%s:%s"
               (emacspeak-google-tool-param tool)
               (emacspeak-google-tool-value tool)))
   toolbelt
   ","))

(defun emacspeak-google-toolbelt ()
  "Returns a newly initialized toolbelt."
  (list
;;; video vid: 1/0
   (make-emacspeak-google-tool
    :name "video"
    :param "vid"
    :range '(0 1)
    :default 0
    :default 0
    :value 0)
;;; Recent
   (make-emacspeak-google-tool
    :name "recent"
    :param "r"
    :range '( 0 1)
    :default 0
    :value 0)
;;; Duration restrict for video
   (make-emacspeak-google-tool
    :name "duration"
    :param "dur"
    :range '("m" "s" "l")
    :default "m"
    :value "m")
;;; Blog mode
   (make-emacspeak-google-tool
    :name "blog"
    :param "blg"
    :range '(0 1)
    :default 0
    :value 0)
;;; Books mode
   (make-emacspeak-google-tool
    :name "books"
    :param "bks"
    :range '(0 1)
    :default 0
    :default 0
    :value 0)
;;; Books viewability
   (make-emacspeak-google-tool
    :name "books-viewability"
    :param "bkv"
    :range '("a" "f")
    :default "a"
    :value "a")
;;; Book Type
   (make-emacspeak-google-tool
    :name "books-type"
    :param "bkt"
    :range '("b" "p" "m")
    :default "b"
    :value "b")
;;; Forums Mode
   (make-emacspeak-google-tool
    :name "forums"
    :param "frm"
    :range '(0 1)
    :default 0
    :value 0)
;;; News Mode
   (make-emacspeak-google-tool
    :name "news"
    :param "nws"
    :range '(0 1)
    :default 0
    :value 0)
;;; Reviews
   (make-emacspeak-google-tool
    :name "reviews"
    :param "rvw"
    :range '(0 1)
    :default 0
    :value 0)
;;; Web History Visited
   (make-emacspeak-google-tool
    :name "web-history-visited"
    :param "whv"
    :range '(0 1)
    :default 0
    :value 0)
;;; Web History Not Visited
   (make-emacspeak-google-tool
    :name "web-history-not-visited"
    :param "whnv"
    :range '(0 1)
    :default 0
    :value 0)
;;; Images
   (make-emacspeak-google-tool
    :name "images"
    :param "img"
    :range '(0 1)
    :default 0
    :value 0)
;;; Structured Snippets
   (make-emacspeak-google-tool
    :name "structured-snippets"
    :param "sts"
    :range '(0 1)
    :default 0
    :value 0)
;;; sort by date
   (make-emacspeak-google-tool
    :name "sort-by-date"
    :param "std"
    :range '(0 1)
    :default 0
    :value 0)
;;; Timeline
   (make-emacspeak-google-tool
    :name "timeline"
    :param "tl"
    :range '(0 1)
    :default 0
    :value 0)
;;; Timeline Low
   (make-emacspeak-google-tool
    :name "timeline-low"
    :param "tll"
    :range "YYYY/MM"
    :default ""
    :value "")
;;; Date Filter
   (make-emacspeak-google-tool
    :name "date-filter"
    :param "qdr"
    :range "tn"
    :default ""
    :value "")
;;; Price
   (make-emacspeak-google-tool
    :name "Price"
    :param "prc"
    :range '(0 1)
    :default 0
    :value 0)
;;; Timeline High
   (make-emacspeak-google-tool
    :name "timeline-high"
    :param "tlh"
    :range "YYYY/MM"
    :default ""
    :value "")
;;; more:commercial
   (make-emacspeak-google-tool
    :name "commercial"
    :param "cpk"
    :range '(0 1)
    :default 0
    :value 0)
;;; less:commercial
   (make-emacspeak-google-tool
    :name "non-commercial" 
    :param "cdcpk"
    :range '(0 1)
    :default 0
    :value 0)))

;;}}}
;;{{{ Interactive Commands

(loop for this-tool in
      (emacspeak-google-toolbelt)
      do
      (eval
       `(defun
          ,(intern
            (format
             "emacspeak-google-toolbelt-change-%s"
             (emacspeak-google-tool-name this-tool)))
          (belt)
          ,(format
            "Change  %s in this Google tool."
            (emacspeak-google-tool-name this-tool))
          (interactive)
          (let*
              (
               (tool
                (find-if #'(lambda (tool) (string-equal (emacspeak-google-tool-name tool)
                                                        ,(emacspeak-google-tool-name this-tool)))
                         belt))
               (param (emacspeak-google-tool-param tool))
               (value (emacspeak-google-tool-value tool))
               (range (emacspeak-google-tool-range tool)))
            (cond
             ((and (listp range)
                   (= 2 (length range)))
;;; toggle value
              (setf (emacspeak-google-tool-value tool)
                    (if (equal value (first range))
                        (second range)
                      (first range))))
             ((listp range)
;;; Prompt using completion
              (setf  (emacspeak-google-tool-value tool)
                     (completing-read
                      "Set tool to: "
                      range)))
             ((stringp range)
              (setf (emacspeak-google-tool-value tool)
                    (read-from-minibuffer  range)))
             (t (error "Unexpected type!")))))))
                                           
                      
                   
;;}}}
;;{{{ Minor mode and keymap

;;;###autoload
(defvar emacspeak-google-keymap nil
  "Keymap used in Google minor mode.")

(define-minor-mode emacspeak-google-mode
  "Google minor mode."
  :keymap  emacspeak-google-keymap
  :lighter " Google")

;;}}}

(provide 'emacspeak-google)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
