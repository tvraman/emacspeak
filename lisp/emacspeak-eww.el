;;; emacspeak-eww.el --- Speech-enable EWW
;;; $Id: emacspeak-eww.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description: Speech-enable EWW An Emacs Interface to eww
;;; Keywords: Emacspeak, Audio Desktop eww
;;{{{ LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;; $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{ Copyright:
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
;;; MERCHANTABILITY or FITNEWW FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction

;;; Commentary:
;;; EWW == Emacs Web Browser
;;; EWW is a light-weight Web browser built into Emacs 24.4.
;;; This module speech-enables EWW

;;; Code:
;;}}}
;;{{{ Required modules

(require 'cl)
(declaim (optimize (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-we)
(require 'emacspeak-webutils)
(require 'xml)
;;}}}
;;{{{ Map Faces To Voices:

(voice-setup-add-map
 '(
   (eww-form-submit voice-animate)
   (eww-form-checkbox voice-monotone)
   (eww-form-select voice-annotate)
   (eww-form-text voice-lighten)))

;;}}}
;;{{{ Advice Interactive Commands:

(loop
 for f in
 '(eww-up-url eww-top-url
              eww-next-url eww-previous-url
              eww-back-url eww-forward-url)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (setq eww-cache-updated nil)
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)
       (dtk-speak eww-current-title)))))

(loop
 for f in
 '(eww eww-reload eww-open-file)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (setq eww-cache-updated nil)
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'open-object)))))

(defadvice eww-render (after emacspeak pre act comp)
  "Setup Emacspeak for rendered buffer."
  (setq eww-cache-updated nil)
  (emacspeak-webutils-run-post-process-hook)
  (rename-buffer eww-current-title 'unique))

(defadvice eww-add-bookmark (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'mark-object)))

(defadvice eww-beginning-of-text (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'large-movement)))

(defadvice eww-end-of-text(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'mark-object)))

(defadvice eww-bookmark-browse (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(defadvice eww-bookmark-kill (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'delete-object)))

(defadvice eww-bookmark-quit (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'close-object)))

(defadvice eww-bookmark-yank(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'yank-object)))

(defadvice eww-list-bookmarks(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'open-object)))

(loop
 for f in
 '(eww-next-bookmark eww-previous-bookmark)
 do
 (eval
  `(defadvice ,f(after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p) (emacspeak-auditory-icon 'select-object))
     (emacspeak-speak-line))))

(defadvice eww-quit(after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p) (emacspeak-auditory-icon 'close-object)))
(loop
 for f in
 '(eww-change-select
   eww-toggle-checkbox
   eww-submit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'button)))))

(loop
 for f in
 '(shr-next-link shr-previous-link)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-region
        (point)
        (next-single-char-property-change (point) 'face nil (point-max)))))))

;;}}}
;;{{{ Setup EWW Initialization:

;;; Inform emacspeak-webutils about EWW:

(add-hook
 'eww-mode-hook
 #'(lambda ()
     (setq
      emacspeak-webutils-document-title
      #'(lambda ()
          (declare (special  eww-current-title))
          eww-current-title)
      emacspeak-webutils-url-at-point
      #'(lambda ()
          (get-text-property (point) 'help-echo))
      emacspeak-webutils-current-url
      #'(lambda ()
          (declare (special eww-current-url))
          eww-current-url))))

(defun emacspeak-eww-setup ()
  "Setup keymaps etc."
  (declare (special eww-mode-map))
  (loop
   for binding  in
   '(
     ("\M-r" rename-buffer)
     ("\C-e" emacspeak-prefix-command)
;;; Capital Letters 
     ("A" eww-view-filtered-dom-by-attribute)
     ("I" eww-view-filtered-dom-by-id)
     ("C" eww-view-filtered-dom-by-class)
     ("E" eww-view-filtered-dom-by-element-list)
     ("R" emacspeak-eww-restore)
;;; lower case letters 
     ("e" emacspeak-we-xsl-map)
     ("f" shr-next-link)
     ("b" shr-previous-link)
     ("*" eww-add-bookmark)
     )
   do
   (emacspeak-keymap-update eww-mode-map binding)))

(when (boundp 'eww-mode-map) (emacspeak-eww-setup))

;;}}}
;;{{{ element, class, role, id caches:

(defvar eww-cache-updated nil
  "Records if caches are updated.")

(make-variable-buffer-local 'eww-cache-updated)

;;; Mark cache to be dirty if we restore history:
(defadvice eww-restore-history (after emacspeak pre act comp)
  "mark cache dirty."
  (setq eww-cache-updated nil))

(defvar eww-id-cache nil
  "Cache of id values. Is buffer-local.")
(make-variable-buffer-local 'eww-id-cache)

(defvar eww-class-cache nil
  "Cache of class values. Is buffer-local.")
(make-variable-buffer-local 'eww-class-cache)

(defvar eww-role-cache nil
  "Cache of role values. Is buffer-local.")

(make-variable-buffer-local 'eww-role-cache)

(defvar eww-element-cache nil
  "Cache of element names. Is buffer-local.")
(make-variable-buffer-local 'eww-element-cache)

(defun eww-update-cache (dom)
  "Update element, role, class and id cache."
  (declare (special eww-element-cache eww-id-cache
                    eww-role-cache eww-class-cache eww-cache-updated))
  (when (listp dom) ; build cache
    (let ((id (xml-get-attribute-or-nil dom 'id))
          (class (xml-get-attribute-or-nil dom 'class))
          (role (xml-get-attribute-or-nil dom 'role))
          (el (symbol-name (xml-node-name dom)))
          (children (xml-node-children dom)))
      (when id (pushnew id eww-id-cache))
      (when class (pushnew class eww-class-cache))
      (when role (pushnew role eww-role-cache))
      (when el (pushnew el eww-element-cache))
      (when children (mapc #'eww-update-cache children)))
    (setq eww-cache-updated t)))

;;}}}
;;{{{ Filter DOM:
(defvar eww-shr-render-functions
  '((title . eww-tag-title)
    (form . eww-tag-form)
    (input . eww-tag-input)
    (textarea . eww-tag-textarea)
    (body . eww-tag-body)
    (select . eww-tag-select)
    (link . eww-tag-link)
    (a . eww-tag-a))
  "Customize shr rendering for EWW.")

(defun eww-filter-dom (dom predicate)
  "Return DOM dom filtered by predicate.
 Predicate receives the node to test."
  (cond
   ((not (listp dom)) nil)
   ((funcall predicate dom) dom)
   (t
    (let ((filtered
           (delq nil
                 (mapcar
                  #'(lambda (node) (eww-filter-dom node predicate))
                  (xml-node-children dom)))))
      (when filtered
        (push (xml-node-attributes dom) filtered)
        (push (xml-node-name dom) filtered))))))

(defun eww-attribute-tester (attr value)
  "Return predicate that tests for attr=value for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (when
            (string= (xml-get-attribute node (quote ,attr)) ,value) node))))

(defun eww-elements-tester (element-list)
  "Return predicate that tests for presence of element in element-list
for use as a DOM filter."
  (eval
   `#'(lambda (node)
        (when (memq (xml-node-name node) (quote ,element-list)) node))))

(defun eww-view-filtered-dom-by-attribute ()
  "Display DOM filtered by specified attribute=value test."
  (interactive)
  (declare (special eww-id-cache eww-class-cache
                    eww-shr-render-functions
                    eww-role-cache eww-cache-updated eww-current-dom))
  (unless (string= (buffer-name) "*eww*") (error "Not in EWW buffer."))
  (unless (and (boundp 'eww-current-dom) eww-current-dom)
    (error "No DOM to filter!"))
  (unless eww-cache-updated (eww-update-cache eww-current-dom))
  (unless
      (or eww-role-cache eww-id-cache eww-class-cache)
    (error "No id/class to filter."))
  (let*
      ((attr-list nil)
       (attr
        (progn
          (when eww-class-cache (push "class" attr-list))
          (when eww-id-cache (push "id" attr-list))
          (when eww-role-cache (push "role" attr-list))
          (read (completing-read "Attr: " attr-list nil 'must-match))))
       (value
        (completing-read
         "Value: "
         (cond
          ((eq attr 'id) eww-id-cache)
          ((eq attr 'class)eww-class-cache)
          ((eq attr 'role)eww-role-cache)
          (t (error "Only filter by class, id or role.")))
         nil 'must-match))
       (inhibit-read-only t)
       (dom (eww-filter-dom eww-current-dom (eww-attribute-tester attr value)))
       (shr-external-rendering-functions eww-shr-render-functions))
    (when dom
      (eww-save-history)
      (eww-setup-buffer)
      (goto-char (point-min))
      (shr-insert-document dom)
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (goto-char (point-min))
      (setq buffer-read-only t))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-buffer)))

(defun eww-view-filtered-dom-by-id ()
  "Display DOM filtered by specified id=value test."
  (interactive)
  (declare (special eww-id-cache eww-cache-updated
                    eww-shr-render-functions eww-current-dom))
  (unless (string= (buffer-name) "*eww*") (error "Not in EWW buffer."))
  (unless (and (boundp 'eww-current-dom) eww-current-dom)
    (error "No DOM to filter!"))
  (unless eww-cache-updated (eww-update-cache eww-current-dom))
  (unless eww-id-cache
    (error "No id to filter."))
  (let*
      ((value
        (completing-read "Value: " eww-id-cache nil 'must-match))
       (inhibit-read-only t)
       (dom (eww-filter-dom eww-current-dom (eww-attribute-tester 'id value)))
       (shr-external-rendering-functions eww-shr-render-functions))
    (when dom
      (eww-save-history)
      (eww-setup-buffer)
      (goto-char (point-min))
      (shr-insert-document dom)
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (goto-char (point-min))
      (setq buffer-read-only t))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-buffer)))

(defun eww-view-filtered-dom-by-class ()
  "Display DOM filtered by specified class=value test."
  (interactive)
  (declare (special eww-class-cache eww-cache-updated
                    eww-shr-render-functions eww-current-dom))
  (unless (string= (buffer-name) "*eww*") (error "Not in EWW buffer."))
  (unless (and (boundp 'eww-current-dom) eww-current-dom)
    (error "No DOM to filter!"))
  (unless eww-cache-updated (eww-update-cache eww-current-dom))
  (unless eww-class-cache
    (error "No class to filter."))
  (let*
      ((value
        (completing-read "Value: " eww-class-cache nil 'must-match))
       (inhibit-read-only t)
       (dom (eww-filter-dom eww-current-dom (eww-attribute-tester 'class value)))
       (shr-external-rendering-functions eww-shr-render-functions))
    (when dom
      (eww-save-history)
      (eww-setup-buffer)
      (goto-char (point-min))
      (shr-insert-document dom)
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (goto-char (point-min))
      (setq buffer-read-only t))
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-buffer)))
(defun eww-view-filtered-dom-by-element-list ()
  "Display DOM filtered by specified el list."
  (interactive)
  (declare (special eww-element-cache
                    eww-shr-render-functions eww-cache-updated eww-current-dom ))
  (unless (string= (buffer-name) "*eww*") (error "Not in EWW buffer."))
  (unless (and (boundp 'eww-current-dom) eww-current-dom)
    (error "No DOM to filter!"))
  (unless eww-cache-updated (eww-update-cache eww-current-dom))
  (let ((el-list nil)
        (el (completing-read "Element: " eww-element-cache
                             nil 'must-match)))
    (loop until (zerop (length el))
          do
          (pushnew (read el) el-list)
          (setq el (completing-read "Element: " eww-element-cache
                                    nil 'must-match)))
    (let ((inhibit-read-only t)
          (dom (eww-filter-dom eww-current-dom (eww-elements-tester el-list)))
          (shr-external-rendering-functions eww-shr-render-functions))
      (when dom
        (eww-save-history)
        (eww-setup-buffer)
        (goto-char (point-min))
        (shr-insert-document dom)
        (set-buffer-modified-p nil)
        (flush-lines "^ *$")
        (goto-char (point-min))
        (setq buffer-read-only t))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-buffer))))

(defun emacspeak-eww-restore ()
  "Restore buffer to pre-filtered canonical state."
  (interactive)
  (declare (special eww-history eww-history-position))
  (eww-restore-history(elt eww-history eww-history-position))
  (emacspeak-speak-mode-line)
  (emacspeak-auditory-icon 'open-object))

;;}}}
;;{{{  Customize image loading:

(defcustom emacspeak-eww-silence-images t
  "Set to nil if you want EWW to load images."
  :type 'boolean
  :group 'emacspeak-eww)

(defadvice eww-display-image (around emacspeak pre act comp)
  "Dont load images if asked to silence them."
  (unless emacspeak-eww-silence-images ad-do-it))
;;}}}
;;{{{ xslt transform on request:

(defadvice eww-display-html (before emacspeak pre act comp)
  "Apply XSLT transform if requested."
  (let ((orig (point)))
    (when (and emacspeak-we-xsl-p emacspeak-we-xsl-transform)
      (emacspeak-xslt-region
       emacspeak-we-xsl-transform (point) (point-max)
       emacspeak-we-xsl-params))
    (goto-char orig)))

;;}}}
(provide 'emacspeak-eww)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
