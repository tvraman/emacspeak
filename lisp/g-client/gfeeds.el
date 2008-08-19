;;; gfeeds.el --- Google Access To Feeds
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  AJAX Feeds -> Lisp
;;; Keywords: Google   AJAX Feeds API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |raman@cs.cornell.edu
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

;;; Fast feed access from Google for use in Emacs

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup gfeeds nil
  "Google Feeds"
  :group 'g)

;;}}}
;;{{{ Variables

(defvar gfeeds-feeds-url
  "http://ajax.googleapis.com/ajax/services/feed/load?q=%s&num=10&v=1.0"
  "URL template for pulling feeds.")

(defvar gfeeds-lookup-url
  "http://ajax.googleapis.com/ajax/services/feed/lookup?q=%s&v=1.0"
  "Rest end-point for feed lookup.")

(defvar gfeeds-find-url
  "http://ajax.googleapis.com/ajax/services/feed/find?q=%s&v=1.0"
  "Rest end-point for finding feeds.")


(defvar gfeeds-referer "http://emacspeak.sf.net"
  "Referer URL to send to the API.")

;;}}}
;;{{{ gfeed Helpers

;;;###autoload
(defsubst gfeeds-feed (feed-url)
  "Return feed structure."
  (declare (special gfeeds-feeds-url gfeeds-referer))
  (let ((json-key-type 'string)
        (result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-feeds-url (g-url-encode feed-url)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get "responseStatus" result))
       (g-json-lookup "responseData.feed" result)))))
;;;###autoload
(defsubst gfeeds-lookup (url)
  "Lookup feed for a given Web page."
  (declare (special gfeeds-lookup-url gfeeds-referer))
  (let ((json-key-type 'string)
        (result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-lookup-url (g-url-encode url)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get "responseStatus" result))
       (g-json-lookup "responseData.url" result)))))
;;;###autoload
(defsubst gfeeds-find (query)
  "Find feeds matching a query."
  (declare (special gfeeds-find-url gfeeds-referer))
  (let ((json-key-type 'string)
        (result nil))
    (g-using-scratch
     (call-process g-curl-program nil t nil
                   "-s"
                   "-e" gfeeds-referer
                   (format gfeeds-find-url (g-url-encode query)))
     (goto-char (point-min))
     (setq result (json-read))
     (when (= 200 (g-json-get "responseStatus" result))
       (g-json-lookup "responseData.entries" result)))))

;;; Feed slot accessors:

(loop for slot in
      '("entries" "type" "description" "author" "link" "title")
      do
      (eval
       `(defsubst ,(intern (format "gfeeds-feed-%s" slot)) (f)
          ,(format "Return %s from feed." slot)
          (cdr (assoc ,slot f)))))

;;}}}
;;{{{ Convenience commands:
;;;###autoload
(defun gfeeds-titles (feed-url)
  "Return list of titles from feed at feed-url."
  (let ((feed (gfeeds-feed feed-url)))
    (when feed
      (loop for article across 
            (gfeeds-feed-entries feed)
            collect (cdr (assoc "title" article))))))

;;}}}
(provide 'gfeeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
;;; gfeeds.el --- Google Feeds
;;;$Id: gcal.el,v 1.30 2006/09/28 17:47:44 raman Exp $
;;; $Author: raman $
;;; Description:  AJAX Feeds -> Lisp
;;; Keywords: Google   AJAX API
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; gcal| T. V. Raman |raman@cs.cornell.edu
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

;;; Provide Google Feed services --- such as 
;;; For use from within Emacs tools.

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'g-utils)

;;}}}
;;{{{ Customizations

(defgroup gfeeds nil
  "Google Feeds"
  :group 'g)

;;}}}

(provide 'gfeeds)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
