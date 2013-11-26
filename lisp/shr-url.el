;;; shr-url.el --- Speech-enable SHR
;;; $Id: emacspeak-shr.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable SHR An Emacs Interface to shr
;;; Keywords: Emacspeak,  Audio Desktop shr
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
;;; MERCHANTABILITY or FITNSHR FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; SHR ==  Simple HTML  Reader
;;; Will work in conjunction with EWWin emacs 24.4 
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(require 'url-parse)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'shr)
(require 'xml)
;;}}}
;;{{{ Enhanced shr:

(defsubst shr-url-get-title-from-dom (dom)
  "Return Title."
  (let ((content dom)
        (title nil))
    (while
        (and content
             (listp content)
             (not
              (setq title
                    (find-if
                     #'(lambda (e) (and (listp e)(eq 'title (car
                                                             e))))
                     (third content)))))
      (setq content (third content)))
    (when title (third title))))
(defsubst shr-url-get-link-text()
  "Return link text at point."
  (let ((url (get-text-property (point) 'shr-url))
        (start nil)
        (end nil))
    (cond
     ((null url)  nil)
     (t
      (setq start (previous-single-property-change (point) 'shr-url)
            end (next-single-property-change (point) 'shr-url))
      (buffer-substring start end)))))

(defvar shr-url-dom nil
  "Buffer local value of DOM.")
(make-variable-buffer-local 'shr-url-dom)

(declaim (special shr-map))
(when (and (boundp 'shr-map) shr-map)
  (loop for k in
        '(
          ("\C-i" shr-next-link)
          ("A" shr-url-view-filtered-dom-by-attribute)
          ("E" shr-url-view-filtered-dom-by-element-list)
          ("o" shr-url-open-link-at-point)
          ([backtab] shr-url-previous-link)
          ("\M-\C-i" shr-previous-link)
          ("q" bury-buffer)
          )
        do
        (emacspeak-keymap-update  shr-map  k)))
(defvar shr-url-this-url nil
  "Buffer local copy of URL of current page.")

(make-variable-buffer-local 'shr-url-this-url)

(defun shr-url-callback (status &rest args)
  "Callback for url-retrieve.
URL  being retrieved is received as part of the callback args."
  (declare (special  shr-map shr-url-dom shr-url-this-url))
  (goto-char (point-min))
  (let*
      ((inhibit-read-only t)
       (start (re-search-forward "^$"))
       (dom (libxml-parse-html-region start(point-max)))
       (buffer (get-buffer-create (or (shr-url-get-title-from-dom dom) "Web"))))
    (with-current-buffer buffer
      (erase-buffer)
      (special-mode)
      (shr-insert-document dom)
      (setq shr-url-dom dom
            shr-url-this-url (first args))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (use-local-map shr-map)
      (setq buffer-read-only t))
    (switch-to-buffer buffer)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-buffer)))

;;;###autoload
(defun shr-url (url &optional display)
  "Display web page."
  (interactive
   (list
    (read-from-minibuffer "URL: "
                          (get-text-property (point) 'shr-url))
    current-prefix-arg))
  (url-retrieve url 'shr-url-callback (list url)))

;;;###autoload

(defun shr-url-open-link-at-point ()
  "Open link under point using shr."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (cond
     ((null url) (message "Not on a link."))
     (shr-base (shr-url (shr-expand-url url)))
     (t (shr-url url)))))

;;;###autoload
(defun shr-url-region (start end)
  "Display region as web page."
  (interactive "r")
  (let* ((inhibit-read-only t)
         (dom (libxml-parse-html-region start end))
         (buffer
          (get-buffer-create
           (or
            (shr-url-get-title-from-dom dom)
            "Untitled"))))
    (with-current-buffer buffer
      (erase-buffer)
      (shr-insert-document dom)
      (setq shr-url-dom dom)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (flush-lines "^ *$")
      (setq buffer-read-only t))
    (switch-to-buffer buffer)
    (emacspeak-speak-mode-line)))

(defun shr-url-next-link ()
  "Move to next link."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (when url (goto-char (next-single-property-change (point) 'shr-url)))
    (setq url (next-single-property-change (point) 'shr-url)); find next link
    (when url (goto-char url))))

(defun shr-url-previous-link ()
  "Move to previous link."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (when url (goto-char (previous-single-property-change (point) 'shr-url)))
    (setq url (previous-single-property-change (point) 'shr-url)); find next link
    (when url (goto-char url))))

;;}}}
;;{{{ class and id caches:

(defvar shr-url-cache-updated nil
  "Records if caches are updated.")

(make-variable-buffer-local 'shr-url-cache-updated)

(defvar shr-url-id-cache nil
  "Cache of id values. Is buffer-local.")
(make-variable-buffer-local 'shr-url-id-cache)

(defvar shr-url-class-cache nil
  "Cache of class values. Is buffer-local.")
(make-variable-buffer-local 'shr-url-class-cache)

(defvar shr-url-element-cache nil
  "Cache of element names. Is buffer-local.")
(make-variable-buffer-local 'shr-url-element-cache)

(defun shr-url-update-cache (dom)
  "Update element, class and id cache."
  (declare (special shr-url-element-cache shr-url-id-cache
                    shr-url-class-cache shr-url-cache-updated))
  (when (listp dom)                         ; build cache
    (let ((id (xml-get-attribute-or-nil dom 'id))
          (class (xml-get-attribute-or-nil dom 'class))
          (el (symbol-name (xml-node-name dom)))
          (children (xml-node-children dom)))
      (when id (pushnew  id shr-url-id-cache))
      (when class (pushnew class shr-url-class-cache))
      (when el (pushnew el shr-url-element-cache))
      (when children (mapc #'shr-url-update-cache children)))))

;;}}}
;;{{{ Filter DOM:

(defun shr-url-filter-dom (dom predicate)
  "Return DOM dom filtered by predicate.
  Predicate receives the node to test."
  (cond
   ((not (listp dom)) nil)
   ((funcall predicate dom) dom)
   (t
    (let ((filtered (delq nil (mapcar
                               #'(lambda (node)
                                   (shr-url-filter-dom node predicate))
                               (xml-node-children dom)))))
      (when filtered
        (push (xml-node-attributes dom) filtered)
        (push (xml-node-name dom) filtered))))))

(defun shr-url-attribute-tester (attr value)
  "Return predicate that tests for attr=value for use as  a DOM filter."
  (eval
   `(defun ,(gensym "shr-url-predicate") (node)
      ,(format "Test if attribute %s has value %s" attr value)
      (when
          (equal (xml-get-attribute node (quote ,attr)) ,value) node))))

(defun shr-url-elements-tester (element-list)
  "Return predicate that tests for presence of element in element-list for use as  a DOM filter."
  (eval
   `(defun ,(gensym "shr-url-predicate") (node)
      ,(format "Test if node  is a member of  %s" element-list)
      (when (member (xml-node-name node) (quote ,element-list)) node))))

;;{{{ Speech-enable:

(loop for f in
      '(shr-url-next-link shr-url-previous-link)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide auditory feedback."
          (when (ems-interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (and (get-text-property (point) 'shr-url)
                 (message (shr-url-get-link-text)))))))

;;}}}

(defun shr-url-view-filtered-dom-by-attribute ()
  "Display DOM filtered by specified attribute=value test."
  (interactive)
  (declare (special shr-url-id-cache shr-url-class-cache
                    shr-url-this-url shr-url-cache-updated shr-dom shr-map))
  (unless (and (boundp 'shr-url-dom) shr-url-dom) (error "No DOM to filter!"))
  (unless shr-url-cache-updated
    (shr-url-update-cache shr-url-dom)
    (setq shr-url-cache-updated t))
  (unless (or shr-url-id-cache shr-url-class-cache) (error "No id/class to filter."))
  (let*
      ((attr (read (completing-read "Attribute: " '("id" "class"))))
       (value (completing-read "Value: " (if (eq attr 'id) shr-url-id-cache shr-url-class-cache)))
       (buffer nil)
       (inhibit-read-only t)
       (url (url-generic-parse-url shr-url-this-url))
       (dom
        (shr-url-filter-dom shr-url-dom (shr-url-attribute-tester attr value))))
    (when dom
      (setq buffer (get-buffer-create "SHR Filtered"))
      (with-current-buffer buffer
        (erase-buffer)
        (goto-char (point-min))
        (special-mode)
        (shr-insert-document dom)
        (setq shr-base
              (concat
               (url-type url)
               "://"
               (url-host url)
               (file-name-directory (url-filename url))))
        (rename-buffer (or (shr-url-get-title-from-dom dom) "Filtered")'unique)
        (setq shr-url-dom dom)
        (set-buffer-modified-p nil)
        (flush-lines "^ *$")
        (use-local-map shr-map)
        (setq buffer-read-only t))
      (switch-to-buffer buffer)
      (emacspeak-auditory-icon 'open0-object)
      (emacspeak-speak-buffer))))

(defun shr-url-view-filtered-dom-by-element-list ()
  "Display DOM filtered by specified el list."
  (interactive)
  (declare (special shr-url-element-cache
                    shr-url-this-url shr-url-cache-updated shr-dom shr-map))
  (unless (and (boundp 'shr-url-dom) shr-url-dom) (error "No DOM to filter!"))
  (unless shr-url-cache-updated
    (shr-url-update-cache shr-url-dom)
    (setq shr-url-cache-updated t))
  (let ((el-list nil)
        (el  (completing-read "Element: " shr-url-element-cache)))
    (loop until (zerop (length  el))
          do
          (pushnew (read el)  el-list)
          (setq el  (completing-read "Element: " shr-url-element-cache)))
    (let
        ((buffer nil)
         (url (url-generic-parse-url shr-url-this-url))
         (inhibit-read-only t)
         (dom (shr-url-filter-dom shr-url-dom (shr-url-elements-tester el-list))))
      (when dom
        (setq buffer (get-buffer-create "SHR Filtered"))
        (with-current-buffer buffer
          (erase-buffer)
          (goto-char (point-min))
          (special-mode)
          (shr-insert-document dom)
          (setq shr-base
                (concat
                 (url-type url)
                 "://"
                 (url-host url)
                 (file-name-directory (url-filename url))))
          (rename-buffer (or (shr-url-get-title-from-dom dom) "Filtered")'unique)
          (setq shr-url-dom dom)
          (set-buffer-modified-p nil)
          (flush-lines "^ *$")
          (use-local-map shr-map)
          (setq buffer-read-only t))
        (switch-to-buffer buffer)
        (emacspeak-auditory-icon 'open-object)
        (emacspeak-speak-buffer)))))

;;}}}
(provide 'emacspeak-shr)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
