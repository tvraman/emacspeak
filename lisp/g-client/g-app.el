;;; g-app.el --- Google Client APP methods
;;;$Id: g-app.el,v 1.14 2006/10/13 01:38:19 raman Exp $
;;; $Author: raman $
;;; Description:  Google Client APP
;;; Keywords: Google   Atom API, Google Services
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; g-client| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Google Services|
;;; $Date: 2006/10/13 01:38:19 $ |
;;;  $Revision: 1.14 $ |
;;; Location undetermined
;;; License: GPL
;;;

;;}}}
;;{{{ Copyright:

;;; Copyright (c) 2006 and later, Google Inc.
;;; All rights reserved.

;;; Redistribution and use in source and binary forms, with or without modification,
;;; are permitted provided that the following conditions are met:

;;;     * Redistributions of source code must retain the above copyright notice,
;;;       this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright notice,
;;;       this list of conditions and the following disclaimer in the documentation
;;;       and/or other materials provided with the distribution.
;;;     * The name of the author may not be used to endorse or promote products
;;;       derived from this software without specific prior written permission.

;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;{{{  introduction

;;; Common Code  For APP
;;; Used by modules like greader, gblogger etc.

;;}}}
;;{{{  Required modules

(require 'cl)
(require 'backquote)
(require 'g-utils)
(require 'g-auth)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{  APP Methods

(define-derived-mode g-app-mode xml-mode
  "Atom  Publishing Interaction"
  "Major mode for APP interaction\n\n
\\{g-app-mode-map"
  (auto-fill-mode 1))

(defvar g-app-publish-action nil
  "This is set up by the various interactive commands to trigger
  the appropriate action when one is ready to publish.")


(make-variable-buffer-local 'g-app-publish-action)

(defun g-app-get-entry (auth-handle url)
  "Retrieve specified entry using credentials in auth-handle.
`url' is the URL of the entry"
  (declare (special g-curl-program g-curl-common-options
                    g-app-this-url))
  (g-auth-ensure-token auth-handle)
  (let ((buffer (get-buffer-create "*atom entry*"))
        (nxml-auto-insert-xml-declaration-flag nil))
    (save-excursion
      (set-buffer buffer)
      (insert
       (g-get-result
        (format
         "%s %s %s  %s 2>/dev/null"
         g-curl-program g-curl-common-options
         (g-authorization auth-handle)
         url)))
      (g-html-unescape-region (point-min) (point-max))
      (g-app-mode)
      (setq g-app-this-url url)
      buffer)))

(make-variable-buffer-local 'g-app-this-url)

(defvar g-app-this-url nil
  "Buffer local variable that records URL we post to.")


(defun g-app-send-buffer (auth-handle http-method)
  "Publish Atom entry in current buffer.
http-method is either POST or PUT"
  (declare (special g-cookie-options
                    g-curl-program g-curl-common-options
                    g-curl-atom-header))
  (unless (and (eq major-mode 'g-app-mode)
               g-app-this-url)
    (error "Not in a correctly initialized Atom Entry."))
  (goto-char (point-min))
  (let ((cl (format "-H Content-length:%s" (buffer-size))))
    (shell-command-on-region
     (point-min) (point-max)
     (format
      "%s %s %s %s %s %s -i -X %s --data-binary @- %s 2>/dev/null"
      g-curl-program g-curl-common-options g-curl-atom-header cl
      (g-authorization auth-handle)
      g-cookie-options
      http-method
      g-app-this-url)
     (current-buffer) 'replace)
    (list (g-http-headers (point-min) (point-max))
          (g-http-body (point-min) (point-max)))))

(defun g-edit-entry (auth-handle url action)
  "Retrieve entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the Edit URL of the entry.
auth-handle is the authorization handle to use.
action is the function to call when we're ready to submit the edit."
  (declare (special g-curl-program g-curl-common-options))
  (let ((buffer (g-app-get-entry url auth-handle)))
    (save-excursion
      (set-buffer buffer)
      (setq g-app-publish-action action)
      (g-xsl-transform-region (point-min) (point-max)
                              g-atom-edit-filter))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (flush-lines "^ *$")
    (goto-char (point-min))
    (search-forward "<content" nil t)
    (forward-line 1)))

;;}}}
(provide 'g-app)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
