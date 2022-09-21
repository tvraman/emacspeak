;;; emacspeak-preamble.el --- standard  include -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; DescriptionEmacspeak Preamble
;; Keywords:emacspeak, audio interface to emacs 
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman
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

;;}}}
;;{{{ Define Locations, Require modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(cl-pushnew (file-name-directory load-file-name) load-path :test #'string-equal)

(eval-when-compile (require 'subr-x))
(require 'advice)
(setq ad-redefinition-action 'accept)

;;{{{  Define locations

(defvar emacspeak-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "emacspeak directory")

;;;###autoload
(defvar emacspeak-lisp-directory
  (expand-file-name  "lisp/" emacspeak-directory)
  "Lisp directory.")

;;;###autoload
(defvar emacspeak-sounds-directory
  (expand-file-name  "sounds/" emacspeak-directory)
  "Auditory icons directory.")

;;;###autoload
(defvar emacspeak-xslt-directory
  (expand-file-name "xsl/" emacspeak-directory)
  "XSLT.")

;;;###autoload
(defvar emacspeak-etc-directory
  (expand-file-name  "etc/" emacspeak-directory)
  "Misc.")

;;;###autoload
(defvar emacspeak-servers-directory
  (expand-file-name  "servers/" emacspeak-directory)
  "Speech servers.")

;;;###autoload
(defvar emacspeak-info-directory
  (expand-file-name  "info/" emacspeak-directory)
  "Info")

;;;###autoload
(defvar emacspeak-user-directory (expand-file-name "~/.emacspeak/")
  "Resources.")

(defvar emacspeak-readme-file
  (expand-file-name "README" emacspeak-directory)
  "README.")

;;;###autoload
(defvar emacspeak-curl-program (executable-find "curl")
  "Curl.")

;;;###autoload
(defvar emacspeak-media-extensions
  (eval-when-compile
    (let
        ((ext
          '("mov" "wma" "wmv" "flv" "m4a" "m4b"  "flac"
            "aiff" "aac" "opus ""mkv"
            "ogv" "oga""ogg" "mp3"  "mp4" "webm" "wav")))
      (concat
       "\\."
       (regexp-opt
        (nconc ext (mapcar #'upcase ext))
        'parens)
       "$")))
  "Media Extensions.")

;;;###autoload
(defvar  emacspeak-m-player-playlist-pattern
  (eval-when-compile
    (concat
     (regexp-opt
      (list ".m3u" ".asx" ".pls" ".rpm" ".ram"))
     "$"))
  "Playlist pattern.")

;;}}}
(require 'dtk-speak)
(require 'voice-setup)
(require 'voice-defs)
(require 'emacspeak-pronounce)
(require 'emacspeak-keymap)
(require 'emacspeak-speak)
(require 'emacspeak-sounds)

;;}}}
;;{{{ Interactive Check Implementation:

;;; Notes:

;; The implementation from 2014 worked for emacspeak.  it has been
;; moved to obsolete/old-emacspeak-preamble.el to avoid the fragility
;; from using backtrace-frame.  See
;; http://tvraman.github.io/emacspeak/blog/ems-interactive-p.html for
;; the version that depended on calling backtrace-frame.

;; This updated implementation avoids that call and was contributed
;; by Stefan Monnier in April 2022.
;; Note that like called-interactively-p, our predicate only returns T
;; for the top-level call, not for any further recursive calls of the
;; function.

;;; Design:
;; Advice on funcall-interactively stores the name of the
;; interactive command being run.
;; The defadvice macro  itself has a defadvice  to generate a locally bound
;; predicate that ensures that ems-interactive-p is only called from
;; within emacspeak advice forms.
;; Thus, ems-interactive-p is reserved for use within Emacspeak advice.

(defvar ems--interactive-fn-name nil
  "Holds name of function being called interactively.")

(defadvice funcall-interactively (around emacspeak  pre act comp)
  "Record name of interactive function being called."
  (let ((ems--interactive-fn-name (ad-get-arg 0)))
    ad-do-it))

;; Beware: Advice on defadvice 
(advice-add 'defadvice :around #'ems--generate-interactive-check)
(defun ems--generate-interactive-check (orig-macro fn-name args &rest body)
  "Lexically redefine ems-interactive-p  to test  ems--interactive-fn-name.
The local definition expands to a call to `eq' that compares
FN-NAME to our stored value of ems--interactive-fn-name."
  (apply
   orig-macro fn-name args
   (macroexp-unprogn
    (macroexpand-all
     (macroexp-progn body)
     ;;  env with new definition
     `((ems-interactive-p
        ;; Reset the var to nil after consuming it to avoid  misfiring if
        ;; fn-name calls itself recursively.
        . ,(lambda ()
             `(when (eq ems--interactive-fn-name ',fn-name)
                (setq ems--interactive-fn-name nil)
                t)))
       . ,macroexpand-all-environment)))))

(defun ems-interactive-p ()
  "Dynamically defined at runtime to provide Emacspeak's
  interactive check.  This definition never be called, so produce debug
  info if the unexpected happens."
  (cl-declare (special ems--interactive-fn-name))
  (error
   (format "From %s: Unexpected call!" ems--interactive-fn-name)))

;;}}}
;;{{{defun: ems--fastload:

;; Internal function  used to efficiently load files.

(defun ems--fastload (file)
  "Load file efficiently."
  (let ((file-name-handler-alist nil)
        (load-source-file-function nil))
    (load file)))

;;}}}
(provide  'emacspeak-preamble)
;;{{{  emacs local variables

;; local variables:
;; folded-file: t
;; end:

;;}}}
