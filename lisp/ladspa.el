;;; ladspa.el --- Ladspa Tools For Emacs  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Expose Ladspa Plugins to Emacs/Emacspeak
;;; Keywords: Emacspeak,  Audio Desktop Ladspa
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2021, T. V. Raman
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
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; This module  uses tools from the Ladspa SDK  to expose
;;; Ladspa plugins in a consistent way to elisp.
;;; The goal is to make it easy to inspect Ladspa Plugins,
;;; And invoke them easily from  Ladspa host applications such as MPlayer.
;;; See
;;; @url{http://emacspeak.blogspot.com/2015/12/a-ladspa-work-bench-for-emacspeak.html}
;;; Some Ladspa Packages that provide plugins:
;;; sudo apt-get install zam-plugins wah-plugins vco-plugins tap-plugins 
;;; swh-plugins rev-plugins mcp-plugins liquidsoap-plugin-ladspa 
;;; ladspa-foo-plugins invada-studio-plugins-ladspa fil-plugins 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(eval-when-compile
  (require 'subr-x)
  (require 'derived))
;;}}}
;;{{{ Structures:

(cl-defstruct ladspa-control
  desc
  min max default
  value)

(cl-defstruct ladspa-plugin
  desc library label controls)

;;}}}
;;{{{ Ladspa Setup:
;;;###autoload
(defconst ladspa-home
  (or (getenv "LADSPA_PATH") "/usr/lib/ladspa")
  "Installation location for Ladspa plugins.")

(defconst ladspa-analyse
  (executable-find "analyseplugin")
  "Analyse plugins tool from Ladspa SDK.")

(defvar ladspa-libs  nil
  "List of installed Ladspa libraries.")

(defun ladspa-libs (&optional refresh)
  "Return list of installed Ladspa libs."
  (cl-declare (special ladspa-libs ladspa-plugins))
  (unless (file-exists-p ladspa-home)
    (error "Ladspa not installed or not configured."))
  (unless (getenv "LADSPA_PATH")(setenv "LADSPA_PATH" ladspa-home))
  (unless ladspa-analyse (error "Ladspa SDK not installed."))
  (cond
   ((and ladspa-libs (null refresh)) ladspa-libs)
   (t
    (setq ladspa-plugins nil)
    (cl-loop
     for d in (split-string ladspa-home ":" t) do
     (setq ladspa-libs (nconc ladspa-libs (directory-files d  nil "\\.so\\'"))))
    ladspa-libs)))

;;}}}
;;{{{ Ladspa Plugins:

(defvar ladspa-plugins nil
  "List of installed plugins with their metadata.")

(defun ladspa-control (c-str)
  "Construct a ladspa control instance from c-str."
  (cl-assert (stringp c-str) nil "Error: c-str is not a string.")
  (let* ((fields (split-string c-str "," 'omit-null))
         (desc (string-trim (cl-first fields)))
         (range
          (when (>= (length fields) 3)
            (split-string (cl-third fields) " " 'omit)))
         (default
           (when (>= (length fields) 4)
             (split-string (cl-fourth fields) " " 'omit)))
         (result (make-ladspa-control)))
    (when (string-match "^Ports:" desc)
      (setq desc (string-trim (substring desc  7))))
    (setf (ladspa-control-desc result) desc
          (ladspa-control-min result) (cl-first range)
          (ladspa-control-max result) (cl-third range)
          (ladspa-control-default result)(cl-second default))
    result))

(defun ladspa-analyse-label (library summary)
  "Analyse Ladspa effect and return a parsed metadata structure."
  (cl-declare (special ladspa-analyse))
  (let* ((label  (substring  summary 0 (string-match " " summary)))
         (desc (string-trim (substring  summary (string-match " " summary))))
         (controls nil)
         (lines (split-string
                 (shell-command-to-string
                  (format
                   "%s   %s %s 2>/dev/null | grep  control "
                   ladspa-analyse library label))
                 "\n" 'omit-null))
         (result (make-ladspa-plugin :library library :label label :desc desc)))
    (cl-loop for c in lines do
             (push (ladspa-control c) controls))
    (setf (ladspa-plugin-controls result) (reverse controls))
    result))

(defun ladspa-analyse-library (library)
  "Analyse Ladspa library and return a
list of parsed ladspa-plugin structures, one per label."
  (cl-declare (special ladspa-analyse))
  (let ((result nil)
        (labels
         (split-string
          (shell-command-to-string
           (format "%s -l %s 2>/dev/null" ladspa-analyse library))
          "\n" 'omit-null)))
    (cl-loop for label in labels  do
             (push (ladspa-analyse-label library label) result))
    (reverse result)))

(defun ladspa-plugins (&optional refresh)
  "Return list of installed Ladspa plugins."
  (cl-declare (special ladspa-plugins))
  (cond
   ((and ladspa-plugins (null refresh)) ladspa-plugins)
   (t
    (setq ladspa-plugins nil)
    (cl-loop
     for library in (ladspa-libs refresh) do
     (setq ladspa-plugins
           (nconc ladspa-plugins (ladspa-analyse-library library))))
    (ladspa-table-init)
    ladspa-plugins)))

;;}}}
;;{{{ Ladspa Table:

(defvar ladspa-table (make-hash-table :test #'eq)
  "Table of Ladspa plugins.")

(defun ladspa-table-init ()
  "Populate Ladspa hash-table."
  (cl-declare (special ladspa-table))
  (cl-loop for p in (ladspa-plugins) do
           (puthash (intern (ladspa-plugin-label p)) p ladspa-table)))

(defun
    ladspa-table-get (label)
  "Return plugin by label."
  (gethash label ladspa-table))

(defun ladspa-read (&optional prompt)
  "Return a plugin after reading its label."
  (cl-declare (special ladspa-table))
  (let ((label
         (intern
          (completing-read
           (or prompt "Ladspa Plugin Tag: ")
           (hash-table-keys ladspa-table)
           nil 'must-match))))
    (when label (gethash label ladspa-table))))

;;}}}
;;{{{ Ladspa Mode:

(defconst ladspa-header-line-format
  '((:eval
     (concat
      (propertize "LAUDIBLE: " 'face 'font-lock-keyword-face)
      (propertize "A Ladspa WorkBench" 'face 'font-lock-string-face)
      (propertize
       (format "%s Effects from %s libraries"
               (length (ladspa-plugins))
               (length (ladspa-libs)))
       'face font-lock-constant-face))))
  "Header line format for SoX buffers.")

(defun ladspa-draw-plugin (p)
  "Draw plugin at point."
  (let ((start (point)))
    (insert (propertize (ladspa-plugin-label p) 'face 'font-lock-keyword-face))
    (insert ":\t")
    (insert
     (propertize (ladspa-plugin-desc p) 'face 'font-lock-string-face))
    (insert "\t")
    (insert (propertize (ladspa-plugin-library p) 'face 'font-lock-constant-face))
    (put-text-property start (point) 'ladspa p))
  (insert "\n"))

(defun  ladspa-init (&optional refresh)
  "Initialize Ladspa."
  (let ((inhibit-read-only  t)
        (plugins (ladspa-plugins refresh)))
    (erase-buffer)
    (cl-loop for  p in plugins do
             (ladspa-draw-plugin p))))

(define-derived-mode ladspa-mode special-mode
  "Interactively manipulate Ladspa filters."
  "A Ladspa workbench for the Emacspeak desktop."
  (setq tab-width 8
        tab-stop-list '(16))

  (setq buffer-read-only t)
  (setq header-line-format ladspa-header-line-format))

;;;###autoload
(defun ladspa (&optional refresh)
  "Ladspa workbench."
  (interactive "P")
  (cl-declare (special ladspa-libs ladspa-plugins))
  (let ((buffer (get-buffer-create "*Ladspa*")))
    (save-current-buffer
      (set-buffer "*Ladspa*")
      (when refresh
        (setq ladspa-plugins nil
              ladspa-libs nil))
      (ladspa-init refresh)
      (goto-char (point-min))
      (ladspa-mode))
    (message "%s plugins in %s libs"
             (length (ladspa-plugins))
             (length (ladspa-libs)))
    (funcall-interactively #'pop-to-buffer buffer)
    ))

(declare-function emacspeak-m-player-add-ladspa "emacspeak-m-player.el")
(declare-function emacspeak-m-player-delete-ladspa "emacspeak-m-player.el")

(cl-declaim (special ladspa-mode-map))
(cl-loop for k in
         '(
           ("RET" ladspa-instantiate)
           ("a" emacspeak-m-player-add-ladspa)
           ("d" emacspeak-m-player-delete-ladspa)
           ("p" previous-line)
           ("n" next-line)
           ("SPC" ladspa-analyse-plugin-at-point)
           ("e" ladspa-edit-control)
           )
         do
         (define-key ladspa-mode-map (ems-kbd (cl-first k)) (cl-second k)))

;;}}}
;;{{{ Instantiate Ladspa Plugin:
(defvar ladspa-edit-help
  (concat
   (propertize "a" 'face 'bold)
   ":\tApply\t\t\t"
   (propertize "e" 'face 'bold)
   ":\t Edit")
  "Help string for  Ladspa Control Edit.")

(defun ladspa-create (plugin)
  "Instantiate plugin  by prompting for control values."
  (let* ((controls (ladspa-plugin-controls plugin)))
    (cl-loop for c in controls do
             (setf (ladspa-control-value c)
                   (read-from-minibuffer
                    (format "%s: Range %s -- %s: Default %s"
                            (ladspa-control-desc c)
                            (ladspa-control-min c) (ladspa-control-max c)
                            (ladspa-control-default c))
                    nil nil nil nil (ladspa-control-default c)))))
  plugin)

(defun ladspa-instantiate ()
  "Instantiate plugin at point by prompting for control values."
  (interactive)
  (cl-declare (special ladspa-edit-help))
  (unless (eq major-mode 'ladspa-mode) (error "This is not a Ladspa buffer"))
  (let ((plugin  (get-text-property (point) 'ladspa)))
    (cond
     ((null plugin) (error "No Ladspa Plugin here."))
     (t
      (let ((inhibit-read-only  t)
            (controls (ladspa-plugin-controls plugin))
            (buffer
             (get-buffer-create  (format "*%s*" (ladspa-plugin-label plugin)))))
        (ladspa-create plugin)
        (save-current-buffer
          (set-buffer buffer)
          (erase-buffer)
          (insert (propertize (ladspa-plugin-desc plugin) 'face 'font-lock-string-face))
          (insert "\n")
          (cl-loop
           for c in controls  and i from 1 do
           (insert
            (propertize (format "%s:  " i)  'face 'font-lock-comment-face))
           (insert
            (propertize (format "%s:" (ladspa-control-desc c))
                        'face font-lock-string-face))
           (insert
            (propertize
             (format "\t\t%s"  (ladspa-control-value c))
             'face 'font-lock-variable-name-face))
           (put-text-property
            (line-beginning-position) (line-end-position)
            'ladspa-control c)
           (insert "\n"))
          (insert "\n")
          (insert ladspa-edit-help)
          (put-text-property (point-min) (point-max)
                             'ladspa plugin)
          (goto-char (point-min))
          (ladspa-mode)
          (setq header-line-format
                (concat
                 "Ladspa: "
                 (propertize (ladspa-plugin-label plugin) 'face 'font-lock-keyword-face))))
        (funcall-interactively #'pop-to-buffer buffer))))))

;;}}}
;;{{{ Edit Ladspa Plugin:

(defun ladspa-edit-control ()
  "Edit Ladspa control  at point by prompting for control values."
  (interactive)
  (unless (eq major-mode 'ladspa-mode) (error "This is not a Ladspa buffer"))
  (unless (get-text-property (point) 'ladspa-control)
    (error "No Ladspa control here."))
  (let* ((inhibit-read-only  t)
         (plugin (get-text-property (point) 'ladspa))
         (control (get-text-property (point) 'ladspa-control)))
    (beginning-of-line)
    (delete-region (line-beginning-position) (line-end-position))
    (setf (ladspa-control-value control)
          (read-from-minibuffer
           (format "%s: Range %s -- %s: Default %s"
                   (ladspa-control-desc control)
                   (ladspa-control-min control) (ladspa-control-max control)
                   (ladspa-control-default control))
           nil nil nil nil
           (ladspa-control-default control)))
    (insert
     (format "%s:  %s:\t%s"
             (1+ (cl-position control (ladspa-plugin-controls plugin)))
             (ladspa-control-desc control) (ladspa-control-value control)))
    (put-text-property
     (line-beginning-position) (line-end-position)
     'ladspa-control control)
    (put-text-property
     (line-beginning-position) (line-end-position)
     'ladspa plugin)
    (goto-char (line-beginning-position))))

;;}}}
;;{{{ Analyse Plugin At Point

(defun ladspa-analyse-plugin-at-point ()
  "Analyse plugin at point."
  (interactive)
  (unless (eq major-mode 'ladspa-mode) (error "This is not a Ladspa buffer"))
  (unless (get-text-property (point) 'ladspa)
    (error "No Ladspa Plugin here."))
  (let ((plugin (get-text-property (point) 'ladspa)))
    (shell-command
     (format "%s %s %s 2>/dev/null"
             ladspa-analyse
             (ladspa-plugin-library plugin) (ladspa-plugin-label plugin)))))

;;}}}
(provide 'ladspa)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
