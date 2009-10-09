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
    :name "Video"
    :param "vid"
    :range '(0 1)
    :value 0)
;;; Recent
   (make-emacspeak-google-tool
    :name "Recent"
    :param "r"
    :range '( 0 1)
    :value 0)
;;; Duration restrict for video
   (make-emacspeak-google-tool
    :name "Duration"
    :param "dur"
    :range '("m" "s" "l")
    :value "m")
;;; Blog mode
   (make-emacspeak-google-tool
    :name "Blog"
    :param "blg"
    :range '(0 1)
    :value 0)
;;; Books mode
   (make-emacspeak-google-tool
    :name "Books"
    :param "bks"
    :range '(0 1)
    :value 0)
;;; Books viewability
   (make-emacspeak-google-tool
    :name "Books Viewability"
    :param "bkv"
    :range '("a" "f")
    :value "a")
;;; Book Type
   (make-emacspeak-google-tool
    :name "Books Type"
    :param "bkt"
    :range '("b" "p" "m")
    :value "b")
;;; Forums Mode
   (make-emacspeak-google-tool
    :name "Forums"
    :param "frm"
    :range '(0 1)
    :value 0)
;;; News Mode
   (make-emacspeak-google-tool
    :name "News"
    :param "nws"
    :range '(0 1)
    :value 0)
;;; Reviews
   (make-emacspeak-google-tool
    :name "Reviews"
    :param "rvw"
    :range '(0 1)
    :value 0)
;;; Web History Visited
   (make-emacspeak-google-tool
    :name "Web History Visited"
    :param "whv"
    :range '(0 1)
    :value 0)
;;; Web History Not Visited
   (make-emacspeak-google-tool
    :name "Web History Not Visited"
    :param "whnv"
    :range '(0 1)
    :value 0)
;;; Images
   (make-emacspeak-google-tool
    :name "Images"
    :param "img"
    :range '(0 1)
    :value 0)
;;; Structured Snippets
   (make-emacspeak-google-tool
    :name "Structured Snippets"
    :param "sts"
    :range '(0 1)
    :value 0)
;;; sort by date
   (make-emacspeak-google-tool
    :name "Sort By Date"
    :param "std"
    :range '(0 1)
    :value 0)
;;; Timeline
   (make-emacspeak-google-tool
    :name "Timeline"
    :param "tl"
    :range '(0 1)
    :value 0)
;;; Timeline Low
   (make-emacspeak-google-tool
    :name "Timeline Low"
    :param "tll"
    :range "YYYY/MM"
    :value "")
;;; Date Filter
   (make-emacspeak-google-tool
    :name "Date Filter"
    :param "qdr"
    :range "tn"
    :value "")
;;; Price
   (make-emacspeak-google-tool
    :name "Price"
    :param "prc"
    :range '(0 1)
    :value 0)
;;; Timeline High
   (make-emacspeak-google-tool
    :name "Timeline High"
    :param "tlh"
    :range "YYYY/MM"
    :value "")))
   
   

  
  
  
;;}}}
;;{{{ Interactive Commands

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
