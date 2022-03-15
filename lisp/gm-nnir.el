;;; gm-nnir.el --- Searching GMail Using NNIR  -*- lexical-binding: t; -*-
;;;$Id: gmaps.el 8157 2013-02-19 01:31:05Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  GMail Search -> IMap -> NNIR -> Gnus
;;; Keywords: GMail, IMap, gnus
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |tv.raman.tv@gmail.com
;;; An emacs interface to Reader|
;;; $Date: 2006/09/28 17:47:44 $ |
;;;  $Revision: 1.30 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;     * Redistributions of source code must retain the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer.  * Redistributions in binary form must reproduce
;;;       the above copyright notice, this list of conditions and the
;;;       following disclaimer in the documentation and/or other
;;;       materials provided with the distribution.  * The name of the
;;;       author may not be used to endorse or promote products
;;;       derived from this software without specific prior written
;;;       permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
;;; THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; Makes search GMail more convenient.
;;; IMap search operators, GMail search extensions.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(with-no-warnings (require 'nnir))
(require 'gnus-group)
;;}}}
;;{{{ IMap Search Operators:

;;; this list is extracted from the IMap RFC 3501
;;; And adds X-GM_RAW  for GMail specific search extensions.
;;; (mapcar 'split-string (split-string
;;; (buffer-substring-no-properties  (point-min)
;;; (point-max)) "\n"))
;;; with imap-search as the current buffer.

(defvar gm-nnir-search-criteria
  '(("BCC" "<string>")
    ("BEFORE" "<date>")
    ("BODY" "<string>")
    ("CC" "<string>")
    ("FROM" "<string>")
    ("HEADER" "<field-name>" "<string>")
    ("KEYWORD" "<flag>")
    ("LARGER" "<n>")
    ("NOT" "<search-key>")
    ("ON" "<date>")
    ("SENTBEFORE" "<date>")
    ("SENTON" "<date>")
    ("SENTSINCE" "<date>")
    ("SINCE" "<date>")
    ("SMALLER" "<n>")
    ("SUBJECT" "<string>")
    ("TEXT" "<string>")
    ("TO" "<string>")
    ("X-GM-RAW" "<string>")
    ("DELETED")
    ("DRAFT")
    ("FLAGGED")
    ("NEW")
    ("RECENT")
    ("UNANSWERED")
    ("UNDELETED")
    ("unseen")
    nil)  
  "IMap search criteria with argument specs.")

(defun gm-nnir-read-imap-clause ()
  "Read one IMap search clause with smart prompts."
  (cl-declare (special gm-nnir-search-criteria))
  (let*
      ((completion-ignore-case t)
       (key (upcase (completing-read "Search Clause" gm-nnir-search-criteria)))
       (args
        (when (cdr (assoc key gm-nnir-search-criteria))
          (read-from-minibuffer
           (mapconcat #'identity (cdr (assoc key gm-nnir-search-criteria))
                      " ")))))
    (if (> (length key) 0)
        (format
         "%s %s" key
         (if args (format "\"%s\"" args) ""))
      "")))

(defun gm-nnir-read-imap-query ()
  "Return query built from a set of clauses."
  (let ((query nil)
        (clause (gm-nnir-read-imap-clause)))
    (while (> (length clause) 0)
      (push clause query)
      (setq clause (gm-nnir-read-imap-clause)))
    (mapconcat #'identity query " ")))

;;}}}
;;{{{ NNIR Engine For GMail 

;;; GMail Search Commands 

;;;###autoload
(defun gm-nnir-group-make-gmail-group (query)
  "Use GMail search syntax exclusively.
See https://support.google.com/mail/answer/7190?hl=en for syntax.
 note: nnimap-address etc are available as local vars if needed
 in these functions."
  (interactive "sGMail Query: ")
  (cl-declare (special nnir-imap-default-search-key))
  (let ((nnir-imap-default-search-key "imap")
        (q (format "X-GM-RAW \"%s\"" query)))
    (cond
     ((gnus-group-group-name)           ; Search current group
      (gnus-group-make-nnir-group
       nil                              ; no extra params needed
       `(nnir-specs (nnir-query-spec (query ,q))))
      (emacspeak-auditory-icon 'open-object)
      (emacspeak-speak-mode-line))
     (t (error "Not on a group.")))))

;;}}}
(provide 'gm-nnir)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-warnings: (not obsolete)
;;; end:

;;}}}
