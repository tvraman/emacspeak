;;; emacspeak-ellama.el --- Speech-enable ELLAMA  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Keywords: Emacspeak,  Audio Desktop ellama
;;; LCD Archive Entry:
;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;; A speech interface to Emacs |
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak


;;;   Copyright:

;; Copyright (C) 1995 -- 2024, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;; ELLAMA ==  Emacs LLM Interaction.
;; ellama uses package llm, and this module speech-enables ellama.

;;; Code:

;;;   Required modules

(eval-when-compile  (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  Interactive Commands:
;; Speech-enable output handlers:

(defadvice ellama-chat-done (after emacspeak pre act comp)
  "speak."
  (let ((dtk-caps nil))
    (dtk-interp-sync)
    (emacspeak-auditory-icon 'item)
    (dtk-speak (ad-get-arg 0))))


(cl-loop
 for f in 
 '(
  ellama-add-code
  ellama-ask
  ellama-ask-about
  ellama-ask-interactive
  ellama-ask-line
  ellama-ask-selection
  ellama-change
  ellama-change-code
  ellama-chat
  ellama-code-add
  ellama-code-complete
  ellama-code-edit
  ellama-code-improve
  ellama-code-review
  ellama-complete
  ellama-complete-code
  ellama-define-word
  ellama-enhance-code
  ellama-enhance-grammar-spelling
  ellama-enhance-wording
  ellama-improve-conciseness
  ellama-improve-grammar
  ellama-improve-wording
  ellama-make-concise
  ellama-make-format
  ellama-make-list
  ellama-make-table
  ellama-render
  ellama-summarize
  ellama-summarize-webpage
  ellama-translate
  )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-object)
       (dtk-speak "Calling LLM")))))


(provide 'emacspeak-ellama)
;;;  end of file
