;;; ladspa.el --- Ladspa Tools For Emacs
;;; $Author: tv.raman.tv $
;;; Description:  Expose Ladspa Plugins to Emacs/Emacspeak
;;; Keywords: Emacspeak,  Audio Desktop Ladspa
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
;;;Copyright (C) 1995 -- 2015, T. V. Raman
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
;;; MERCHANTABILITY or FITNSOX FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; This module  uses tools from the Ladspa SDK  to expose
;;; Ladspa plugins in a consistent way to elisp.
;;; The goal is to make it easy to inspect Ladspa Plugins,
;;; And invoke them easily from within Ladspa host applications such as MPlayer.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Structures:

(cl-defstruct ladspa-control
  desc
  min max default
  value )

(cl-defstruct ladspa-plugin
  desc library label controls)

;;}}}
;;{{{ Ladspa Setup:

(defconst ladspa-home
  (or (getenv "LADSPA_PATH") "/usr/lib/ladspa")
  "Instalation location for Ladspa plugins.")

(defconst ladspa-analyse
  (executable-find "analyseplugin")
  "Analyse plugins tool from Ladspa SDK.")

(defvar ladspa-libs  nil
  "List of installed Ladspa libraries.")

(defun ladspa-libs (&optional refresh)
  "Return list of installed Ladspa libs."
  (declare (special ladspa-libs))
  (cond
   ((and ladspa-libs (null refresh)) ladspa-libs)
   (t
    (loop
     for d in (split-string ladspa-home ":" t) do
     (setq ladspa-libs (nconc ladspa-libs (directory-files d  nil "\\.so$"))))
    ladspa-libs)))

;;}}}
;;{{{ Ladspa Plugins:

(defvar ladspa-plugins nil
  "List of installed plugins with their metadata.")

(defun ladspa-analyse-label (library label)
  "Analyse Ladspa effect and return a parsed metadata structure."
  (let ((result (make-ladspa-plugin :library library :label label)))
    result))

(defun ladspa-analyse-library (library )
  "Analyse Ladspa library and return a list of parsed data."
  (let ((result nil)
        (labels
         (mapcar
          #'(lambda (p) (first (split-string p " ")))
          (split-string
           (shell-command-to-string (format "analyseplugin -l %s" library)) "\n"))))
    (loop
     for label in labels  do
     (push (ladspa-analyse-label library label) result))
    result))


(defun ladspa-plugins (&optional refresh)
  "Return list of installed Ladspa plugins."
  (declare (special ladspa-plugins))
  (cond
   ((and ladspa-plugins (null refresh)) ladspa-plugins)
   (t
    (loop
     for library in (ladspa-libs) do
     (setq ladspa-plugins
           (nconc ladspa-plugins (ladspa-analyse-library library))))
    ladspa-plugins)))

;;}}}
(provide 'ladspa)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
