;;; dom-addons.el --- dom.el addons
;;; $Id: emacspeak-<skeleton>.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable <SKELETON> An Emacs Interface to <skeleton>
;;; Keywords: Emacspeak,  Audio Desktop <skeleton>
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
;;; MERCHANTABILITY or FITN<SKELETON> FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Useful additional functions for dom.el
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'dom)

;;}}}
;;{{{  Filterring Inspired by dom.el:

(defun dom-by-tag-list (dom tag-list)
  "Return elements in DOM that is of type appearing in tag-list.
A tag is a symbol like `td'."
  (let ((matches (cl-loop for child in (dom-children dom)
			  for matches = (and (not (stringp child))
					     (dom-by-tags child tag-list))
			  when matches
			  append matches)))
    (if (member (dom-tag dom) tag-list)
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
	     (find-if #'(lambda (match) (string-match match attr)) match-list))
	(cons dom matches)
      matches)))

(defun dom-by-id-list (dom match-list)
  "Return elements in DOM that have an ID that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'id match-list))

(defun dom-by-class-list (dom match-list)
  "Return elements in DOM that have a class name that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'class match-list))

(defun dom-by-role-list (dom match-list)
  "Return elements in DOM that have a role name that matches regexp MATCH."
  (dom-elements-by-matchlist dom 'role match-list))

;;}}}
(provide 'dom-addons)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
