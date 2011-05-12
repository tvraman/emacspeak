;;; gwis.el --- Google Web Incremental Search For Emacs
;;;$Id: gwis.el,v 1.5 2006/01/13 23:18:23 raman Exp raman $
;;; $Author: raman $
;;; Description:  Web Incremental Search via Google
;;; Keywords: Web Incremental Search Google
;;{{{  LCD:

;;; LCD Archive Entry:
;;; gwis| T. V. tv.raman.tv@gmail.com
;;; An emacs interface that implements web incremental search
;;; $Date: 2006/01/13 23:18:23 $ |
;;;  $Revision: 1.5 $ |
;;; Location undetermined

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
;;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERGWIS) ARISING IN ANY
;;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;}}}
;;{{{  introduction

;;; Commentary:

;;; Create a special mode that progressively performs a Google
;;;search using words in the buffer.

;;; Usage: 

;;; M-x gwis creates a gwis buffer.
;;; type into it as usual;  When following conditions are true:
;;; Emacs has been idle for gwis-idle-delay seconds,
;;; Contents of the Gwis buffer have changed since last search,
;;; and the Gwis buffer is still selected, 
;;; gwis-mode will automatically launch a google search with the
;;; contents of the gwis buffer as the query.

;;; Default is to perform a Web search:
;;; gwis-select-searcher --- bound to [tab] by default 
;;; can be used to select other searches such as news.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(require 'derived)

;;}}}
;;{{{  Variables:

(defcustom gwis-search-table
  '((web . "http://www.google.com/search?q=%s&num=25")
    (image
     . "http://images.google.com/images?hl=en&tab=wi&ie=UTF-8&q=%s")
    (blog . "http://blogsearch.google.com/blogsearch?hl=en&q=%s")
    (news . "http://news.google.com/news?hl=en&ned=tus&q=%s")
    (froogle . "http://froogle.google.com/froogle?q=%s/")
    (local . "http://local.google.com/local?q=%s"))
  "Table mapping search-type to URI."
  :type '(repeat  :tag "Avaialble Searches"
                  (cons :tag "Search"
                        (symbol :tag "Type")
                        (string :tag "URI")))
  :group 'gwis)

(defcustom gwis-search-type 'web
  "Default Gwis search type."
  :type  (list 'choice
               (loop for s in gwis-search-table
                     collect
                     (list 'const
                           :tag (symbol-name (car s))
                           (car s))))
  :group 'gwis)

(make-variable-buffer-local 'gwis-search-type)

;;}}}
;;{{{  helpers:

(defsubst gwis-get-search-uri (search-type)
  "Return URI for specified search."
  (declare (special gwis-search-table))
  (cdr (assoc  search-type gwis-search-table)))

(defvar gwis-query-string-cache  ""
  "Cached query string.")

(defsubst gwis-get-query-string ()
  "Return query string from gwis buffer."
  (save-excursion
  (let ((start nil))
    (goto-char (point-min))
    (skip-syntax-forward " ")
    (setq start (point))
    (goto-char (point-max))
    (skip-syntax-backward " ")
    (buffer-substring-no-properties start (point)))))

;;}}}
;;{{{ gwis mode:
(defvar gwis-idle-timer nil
  "Idle timer used in gwis mode.")

(defcustom gwis-idle-delay 0.5
  "Idle delay before launching Web Incremental Search. Accepts floating point values."
  :type 'number
  :group 'gwis)

(define-derived-mode gwis-mode  text-mode
  "Web Incremental Search"
  "GWIS --- Web Incremental Search for Emacs

 This mode can be thought of as the Web equivalent of
isearch-mode.  Contents of the Gwis buffer are progressively
added or subtracted from a running Google query and the results
are updated continuously to provide the Web equivalent of Emacs'
`incremental search'.  Unlike incremental-search where individual
keystrokes update the query string, this mode updates the query
string via an idle-timer that runs whenever Emacs has been idle
for more than gwis-idle-delay, and the current buffer is a Gwis
buffer. Once a query has been executed, the query string is
cached to avoid repeating the query.  

The default is to perform a Google Web search; hitting
\\[gwis-select-searcher] bound to command `gwis-select-searcher'
can be used to switch to News or Image search.
Switching to a new searcher clears the query cache so that a new
search is performed."
  (setq gwis-idle-timer
        (run-with-idle-timer gwis-idle-delay 5 'gwis-isearch)))

;;}}}
;;{{{ Commands:

(defun gwis-isearch ()
  "Perform gwis search."
  (interactive)
  (declare (special gwis-query-string-cache
                    gwis-search-type))
  (when (eq major-mode 'gwis-mode)
    (let ((query (gwis-get-query-string)))
      (unless(or  (string-equal  query gwis-query-string-cache)
                  (= (length query) 0))
        (setq gwis-query-string-cache query)
        (browse-url
         (format  (gwis-get-search-uri gwis-search-type)
                 (webjump-url-encode query)))))))

(defun gwis-select-searcher (search-type)
  "Select Gwis searcher ."
  (interactive
   (list
    (intern
     (completing-read "Search Type: "
                      gwis-search-table
                      nil 'must-match))))
  (declare (special gwis-search-type))
  (setq gwis-search-type search-type)
  (setq gwis-query-string-cache ""))

  
(defun gwis ()
  "Create and initialize a `gwis' buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Gwis Interaction*")))
    (save-excursion
      (set-buffer  buffer)
      (erase-buffer)
      (gwis-mode)
      (gwis-setup-keys))
    (switch-to-buffer buffer)))

(defun gwis-setup-keys ()
  "Setup key bindings in gwis mode."
  (declare (special gwis-mode-map))
  (loop for binding  in
        '(
          ("\C-i" gwis-select-searcher)
          )
        do
        (define-key gwis-mode-map
          (first binding) (second binding))))

;;}}}
(provide 'gwis)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
