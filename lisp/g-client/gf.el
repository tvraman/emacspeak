;;; gf.el --- Google Freebase API
;;;$Id: gf.el 8387 2013-08-18 00:02:05Z tv.raman.tv $
;;; $Author: raman $
;;; Description:  AJAX FreeBase API  -> Lisp
;;; Keywords: Google   Freebase AJAX API
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
;;{{{  introduction
;;; Commentary:
;;; Explore Freebase graph search results

;;; Documentation: https://developers.google.com/freebase/v1/search-overview
;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'json)
(require 'g-utils)

;;}}}
;;{{{ Variables

(defvar gf-base-url
  "https://www.googleapis.com/freebase/v1/"
  "Base URL for Freebase API end-point.")

(defvar gf-search-url
  (format "%ssearch" gf-base-url)
  "REST end-point for Freebase searches.")
(defvar gf-topic-url
  (format "%stopic" gf-base-url)
  "REST  end-point for Freebase topic lookup.")

;;}}}
;;{{{ Freebase Search
(defun gf-search-results (query)
  "Return Freebase search results as a parsed JSON."
  (let ((result
         (g-json-get
          'result
          (g-json-get-result
           (format "%s -s '%s'"
                   g-curl-program
                   (format "%s?query=%s" gf-search-url query))))))
    (loop
     for a across result
     when (g-json-get 'id a)
     collect
     (list (g-json-get 'name a)
            (g-json-get 'id a)))))


(defun gf-topic-description (topic)
  "Return topic description."
  (declare (special gf-topic-url))
  (let ((root nil)
        (desc nil)
        (citation nil)
        (entry
         (g-json-get-result
          (format
           "%s -s '%s'"
           g-curl-program
           (format "%s/%s?filter=/common/topic/description"
                   gf-topic-url topic)))))
    (setq root
          (g-json-path-lookup
           "property./common/topic/description.values.[0]" entry))
    (setq desc (g-json-get 'value root))
    (setq citation (g-json-lookup "citation.uri" root))
    (put-text-property 0 (length desc) 'link  citation  desc)
    desc))

;;}}}
(provide 'gf)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
