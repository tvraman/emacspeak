;;; dom-addons.el --- dom.el addons  -*- lexical-binding: t; -*-
;;; $Id: emacspeak-<skeleton>.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable <SKELETON> An Emacs Interface to <skeleton>
;;; Keywords: Emacspeak,  Audio Desktop <skeleton>
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
;;; MERCHANTABILITY or FITN<SKELETON> FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Useful additional functions for dom.el
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'dom)
(require 'g-utils)
;;}}}
;;{{{ Additional helpers:
(defun dom-alternate-links (dom)
  "Return link elements specifying rel=alternate."
  (cl-remove-if-not
   #'(lambda (l) (equal "alternate"
                        (dom-attr l 'rel)))
   (dom-by-tag dom 'link)))

(defun dom-html-add-base (dom base)
  "Add base to HTML dom.
Added element goes inside the HTML head if any."
  (let ((b `(base ((href . ,base))))
        (head (dom-child-by-tag dom 'head)))
    (cond
     (head (dom-add-child-before  head b))
     (t (dom-add-child-before dom `(head nil ,b))))
    dom))

(defun dom-html-from-nodes (nodes &optional base)
  "Make  an HTML DOM having nodes as children unless nodes is an HTML document."
  (let ((dom
         (cond
          ((not (eq 'html (dom-tag nodes)))
           (apply #'dom-node 'html nil nodes))
          (t nodes))))
    (when base (dom-html-add-base dom  base))
    dom))

;;}}}
;;{{{  Filtering Inspired by dom.el:

(defun dom-by-tag-list (dom tag-list)
  "Return elements in DOM that is of type appearing in tag-list.
A tag is a symbol like `td'."
  (let ((matches
         (cl-loop
          for child in (dom-children dom)
          for matches =
          (and (not (stringp child))
               (dom-by-tag-list child tag-list))
          when matches append matches)))
    (if (memq (dom-tag dom) tag-list)
        (cons dom matches)
      matches)))

(defun dom-elements-by-matchlist (dom attribute match-list)
  "Find elements matching match-list (a list of regexps) in ATTRIBUTE.
ATTRIBUTE would typically be `class', `id' or the like."
  (let ((matches
         (cl-loop
          for child in (dom-children dom)
          for matches =
          (and
           (not (stringp child))
           (dom-elements-by-matchlist child attribute match-list))
          when matches append matches))
        (attr (dom-attr dom attribute)))
    (if (and attr
             (cl-find-if #'(lambda (match) (string-match match attr)) match-list))
        (cons dom matches)
      matches)))

(defun dom-by-id-list (dom match-list)
  "Return elements in DOM that have an ID that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'id match-list))

(defun dom-by-class-list (dom match-list)
  "Return elements in DOM that have a class name that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'class match-list))

(defun dom-by-role (dom match)
  "Return elements in DOM that have a role name that matches regexp MATCH."
  (dom-elements dom 'role match))

(defun dom-by-role-list (dom match-list)
  "Return elements in DOM that have a role name that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'role match-list))

(defun dom-by-property (dom match)
  "Return elements in DOM that have a property name that matches regexp MATCH."
  (dom-elements dom 'property match))

(defun dom-by-property-list (dom match-list)
  "Return elements in DOM that have a property name that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'property match-list))

(defun dom-by-itemprop (dom match)
  "Return elements in DOM that have a itemprop name that matches regexp MATCH."
  (dom-elements dom 'itemprop match))

(defun dom-by-itemprop-list (dom match-list)
  "Return elements in DOM that have a itemprop name that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'itemprop match-list))

;;}}}
;;{{{DOM From URL:
(defun dom-from-url (url)
  "Return DOM for HTML content at URL."
  (cl-declare (special g-curl-program g-curl-common-options))
  (g-using-scratch
      (shell-command
       (format "%s %s '%s'"
               g-curl-program g-curl-common-options url)
       (current-buffer))
    (goto-char (point-min))
    (xml-remove-comments (point-min) (point-max))
    (libxml-parse-html-region (point-min) (point-max))))

;;}}}
;;{{{dom-node-as-text
(defsubst dom-node-as-text (node)
  "Return all the text bits in the current node and some specific
children, e.g. `a', concatenated."
  (with-temp-buffer (shr-insert-document node)
                    (buffer-substring-no-properties (point-min) (point-max))))

;;}}}
(provide 'dom-addons)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
