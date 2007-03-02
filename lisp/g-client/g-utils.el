;;; g-utils.el --- Google Client Utilities
;;;$Id: g-utils.el,v 1.14 2006/10/13 01:38:19 raman Exp $
;;; $Author: raman $
;;; Description:  Google Client utilities
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

;;; Common Code  e.g. helper functions.
;;; Used by modules like greader, gblogger etc.

;;}}}
;;{{{  Required modules

(require 'cl)
(require 'backquote)
(require 'json)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Customizations:

(defvar g-directory (and load-file-name
                                  (file-name-directory load-file-name))
  "Directory where Google Client is installed.")

(defvar g-scratch-buffer" *g scratch*"
"Scratch buffer we do authentication work.")

(defcustom g-curl-program "curl"
  "Name of CURL executable."
  :type 'string
  :group 'g)

(defcustom g-atom-view-xsl
  (expand-file-name "atom-view.xsl" g-directory)
  "XSLT transform to convert Atom feed to HTML."
  :type 'string
  :group 'g)

(defcustom g-atom-edit-filter
  (expand-file-name "blogger-edit-post.xsl" g-directory)
  "XSLT transform used to tidy up an entry before posting.
For now, this is blogger specific."
  :type 'string
  :group 'g)

(defcustom g-curl-common-options
  "--compressed --silent --location --location-trusted"
  "Common options to pass to all Curl invocations."
  :type 'string
  :group 'g)

(defcustom g-html-handler 'browse-url-of-buffer
  "Function that processes HTML.
Receives buffer containing HTML as its argument."
  :type '(choice
          (function-item browse-url-of-buffer)
          (function-item switch-to-buffer)

          (function :format "%t %v" :tag "Custom:"))
  :group 'g)

(defcustom g-url-under-point 'browse-url-url-at-point
  "Function used to get URL from current context."
  :type '(choice
          (function-item browse-url-url-at-point)
          (function-item w3-view-this-url)
          (function :format "%t %v" :tag "Custom:"))
  :group 'g)

(defcustom g-xslt-program "xsltproc"
  "XSLT Processor."
  :type 'string
  :group 'g)

(defcustom g-cookie-jar
   (expand-file-name "~/.g-cookie-jar")
   "Cookie jar used for Google services.
Customize this to live on your local disk."
   :type 'file
   :set '(lambda (sym val)
          (declare (special g-cookie-options))
          (setq g-cookie-options
                 (format "--cookie %s --cookie-jar %s"
                                 val val))
          (set-default sym val))
   :group 'g)

(defvar g-cookie-options
 (format "--cookie %s --cookie-jar %s"
                                 g-cookie-jar g-cookie-jar)
 "Options to pass for using our cookie jar.")

(defcustom g-curl-debug nil
  "Set to T to see Curl stderr output."
  :type 'boolean
  :group 'g)

(defcustom g-xslt-debug nil
  "Set to T to see xsltproc  stderr output."
  :type 'boolean
  :group 'g)

;;}}}
;;{{{ debug helpers

(defsubst g-curl-debug ()
  "Determines if we show stderr output."
  (declare (special g-curl-debug))
  (if g-curl-debug
" 2>/dev/null"
""))

(defsubst g-xslt-debug ()
  "Determines if we show stderr output."
  (declare (special g-xslt-debug))
  (if g-xslt-debug
" 2>/dev/null"
""))

;;}}}
;;{{{ url encode:

(defsubst g-url-encode (str)
  "URL encode  string."
  (mapconcat '(lambda (c)
		(cond ((= c 32) "+")
		      ((or (and (>= c ?a) (<= c ?z))
			   (and (>= c ?A) (<= c ?Z))
			   (and (>= c ?0) (<= c ?9)))
		       (char-to-string c))
		      (t (upcase (format "%%%02x" c)))))
	     str
	     ""))

;;}}}
;;{{{ transform region

(defsubst g-xsl-transform-region (start end xsl)
  "Replace region by result of transforming via XSL."
  (declare (special g-xslt-program))
  (shell-command-on-region
   start end
   (format "%s %s - %s"
           g-xslt-program xsl (g-xslt-debug))
   'replace))

;;}}}
;;{{{ html unescape

(defvar g-html-charent-alist
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&apos;" . "'") ("&amp;" . "&"))
  "Alist of HTML character entities to unescape.")

(defsubst g-html-unescape-region (start end)
  "Unescape HTML entities."
  (declare (special g-html-charent-alist))
  (save-excursion
    (loop for entry in g-html-charent-alist
          do
          (let ((entity (car  entry))
                (replacement (cdr entry )))
            (goto-char start)
            (while (search-forward entity end t)
              (replace-match replacement ))))))

(defsubst g-html-escape-region (start end)
  "Escape HTML entities."
  (declare (special g-html-charent-alist))
  (save-excursion
    (loop for entry in g-html-charent-alist
          do
          (let ((entity (cdr  entry))
                (replacement (car entry )))
            (goto-char start)
            (while (search-forward entity end t)
              (replace-match replacement ))))))

;;}}}
;;{{{ json conveniences:

(defsubst g-json-get (key object)
  "Return object.key from json object or nil if not found."
  (cdr (assoc key object)))

(defalias 'g-json-aref 'aref)

;;}}}
;;{{{ helper macros

(defmacro g-using-scratch(&rest body)
  "Evaluate forms in a  ready to use temporary buffer."
  `(progn
     (declare (special g-scratch-buffer))
  (let ((buffer (get-buffer-create g-scratch-buffer))
        (buffer-undo-list t))
    (save-excursion
      (set-buffer  buffer)
      (kill-all-local-variables)
      (erase-buffer)
      (progn ,@body)))))

(defsubst g-get-result (command)
  "Run command and return its output."
  (g-using-scratch
      (shell-command command (current-buffer) 'replace)
      (buffer-string)))

(defsubst g-json-get-result(command)
  "Get command results and return json object read from result
string."
  (json-read-from-string
   (g-get-result command)))

(defsubst g-display-result (command style)
  "Display result retrieved by command using specified style.
Typically, content is pulled using Curl , converted to HTML using style  and
  previewed via `g-html-handler'."
  (declare (special g-xslt-program g-html-handler))
  (g-using-scratch
      (shell-command command (current-buffer))
      (when style
        (g-xsl-transform-region (point-min) (point-max) style))
      (funcall g-html-handler (current-buffer))))

(defsubst g-display-xml-string (string style)
  "Display XML string  using specified style.
XML string is transformed via style
  and previewed via `g-html-handler'."
  (declare (special g-xslt-program g-html-handler))
  (g-using-scratch
   (insert string )
   (when style
     (g-xsl-transform-region (point-min) (point-max) style))
   (funcall g-html-handler (current-buffer))))

;;}}}
;;{{{  HTTP Headers:

(defvar g-crlf-pair
  (format "%c%c"  10  10)
  "HTTP headers are ended by a CRLF pair.
Note that in the Curl output, we see lf rather than crlf.")

(defsubst g-http-headers (start end)
  "Parse HTTP headers in region and return an alist."
  (declare (special g-crlf-pair))
  (goto-char start)
  (when (search-forward g-crlf-pair end 'no-error )
    (setq end (point)))
  (save-restriction
    (narrow-to-region start end)
    (let ((headers nil)
          (pos nil)
          (fields nil))    (goto-char (point-min))
          (when (looking-at "HTTP/[0-9.]+")
            (skip-syntax-forward "^ ")
            (skip-syntax-forward " ")
            (setq pos (point))
            (skip-syntax-forward "^ ")
            (push
             (cons "Status"
                   (buffer-substring-no-properties
                    pos (point)))
             headers)
            (forward-line 1))
          (while (not (eobp))
            (setq fields
                  (split-string (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))
                                ": "))
            (when (= 2 (length fields))
              (push
               (cons (first fields) (second fields))
               headers))
            (forward-line 1))
          headers)))

(defsubst g-http-body (start end)
  "Return body from HTTP response."
  (declare (special g-crlf-pair))
  (goto-char start)
  (cond
   ((search-forward g-crlf-pair end 'no-error )
    (buffer-substring-no-properties (point) end))
   (t "")))

(defsubst g-http-header (name header-alist)
  "Return specified header from headers-alist."
    (when (assoc name header-alist) (cdr (assoc name header-alist))))

;;}}}
(provide 'g-utils)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
