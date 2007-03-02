;;; g-autogen.el --- Generate autoloads for G
;;; $Id: g-autogen.el,v 1.4 2006/08/18 22:06:58 raman Exp $
;;; $Author: raman $
;;; Description:  autoload Wizard for the G client
;;; Keywords: Google Services
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; g-client| T. V. Raman |raman@cs.cornell.edu
;;; An emacs interface to Google Services|
;;; $Date: 2006/08/18 22:06:58 $ |
;;;  $Revision: 1.4 $ |
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

;;{{{ Introduction:

;;; Generate autoloads from autoload cookies

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'autoload)
(load-library "cus-dep")

;;}}}
;;{{{ Variables

(defvar g-directory (and load-file-name
                         (file-name-directory load-file-name))
  "Directory where Google Client is installed.")

(defvar g-autogen-autoloads-file
  (expand-file-name "g-loaddefs.el" g-directory)
  "File that holds automatically generated autoloads for G.")

;;}}}
;;{{{ generate autoloadms

(defvar g-autogen-updator
  (cond
   ((fboundp 'update-autoloads-from-directories)
    'update-autoloads-from-directories)
   ((fboundp  'update-directory-autoloads)
    'update-directory-autoloads))
  "Function used to extract autoloads.")

(defun g-autogen-generate-autoloads ()
  "Generate G autoloads."
  (declare (special  g-autogen-autoloads-file
                     g-autogen-updator g-directory))
  (let ((generated-autoload-file g-autogen-autoloads-file))
    (funcall g-autogen-updator g-directory)))

;;}}}
;;{{{  generate custom autoloads:

(defvar g-autogen-custom-dependencies-file
  (expand-file-name "g-cus-load.el" g-directory)
  "File that holds custom dependency information.")

(defun g-autogen-custom-make-dependencies ()
  "Generate G custom dependencies."
  (declare (special  g-autogen-custom-dependencies-file))
  (let ((generated-custom-dependencies-file g-autogen-custom-dependencies-file))
    (custom-make-dependencies)))

;;}}}
(provide 'g-autogen)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
