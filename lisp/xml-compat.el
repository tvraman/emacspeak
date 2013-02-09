;;; xml-compat.el --- Compatibility  for xml.el and libxml
;;; $Id: xml-compat>.el 4797 2007-07-16 23:31:22Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Compatibility  layer for cutting over to xml.el
;;; and libxml 
;;; Keywords: Emacspeak,  Audio Desktop xml
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
;;;Copyright (C) 1995 -- 2011, T. V. Raman
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
;;; Defines needed compatibility layer so we can move from
;;;xml-parse.el to xml.el with libxml  if available 

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{  Compatibility Functions 

(defsubst xml-tag-child (node name)
  "Return first child matching name."
  (first (xml-get-children node name)))

;;;###autoload 
(cond
 ((fboundp 'libxml-parse-xml-region)
  (defalias 'xml-parse-xml-region 'libxml-parse-xml-region))
 (t
  (defun xml-parse-xml-region (start end)
    "Parse region using xml-parse."
    (car (xml-parse-region start end)))))
;;}}}

(provide 'xml-compat)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
