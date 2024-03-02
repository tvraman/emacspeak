;;; emacspeak-preamble.el --- standard  include -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; DescriptionEmacspeak Preamble
;; Keywords:emacspeak, audio interface to emacs
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;;
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;;

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

;;; Commentary:
;; Module that is preloaded by every emacspeak module.
;; 1.  Defines key macros.
;; 2. Defines location-related variables.
;; Define locations of executables.;;; Code:

;;;  cl:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(cl-pushnew (file-name-directory load-file-name) load-path :test #'string=)

(require 'advice)
(put 'defadvice 'byte-obsolete-info nil)
(setq ad-redefinition-action 'accept)

;;;   Define locations:

(defconst emacspeak-directory
  (expand-file-name "../" (file-name-directory load-file-name))
  "emacspeak directory")

(defconst emacspeak-lisp-directory
  (expand-file-name  "lisp/" emacspeak-directory)
  "Lisp directory.")

(defconst emacspeak-sounds-dir
  (expand-file-name  "sounds/" emacspeak-directory)
  "Auditory icons directory.")

(defconst emacspeak-xslt-directory
  (expand-file-name "xsl/" emacspeak-directory)
  "XSLT.")

(defconst emacspeak-etc-directory
  (expand-file-name  "etc/" emacspeak-directory)
  "Misc.")

(defconst emacspeak-servers-directory
  (expand-file-name  "servers/" emacspeak-directory)
  "Speech servers.")

(defconst emacspeak-info-directory
  (expand-file-name  "info/" emacspeak-directory)
  "Info")

(defconst emacspeak-user-directory (expand-file-name "~/.emacspeak/")
  "Resources.")

(defconst emacspeak-readme-file
  (expand-file-name "README" emacspeak-directory)
  "README.")

(defconst emacspeak-icon
  (expand-file-name "emacspeak.ogg" emacspeak-sounds-dir)
  "Emacspeak startup icon.")

;; local media dir
(defconst emacspeak-media (getenv "XDG_MUSIC_DIR")
  "Local media directory.")
(defconst  emacspeak-media-shortcuts
  (expand-file-name "media/radio/" emacspeak-directory)
  "Directory where we organize   and media shortcuts. ")
(defconst emacspeak-media-extensions
  (eval-when-compile
    (let
        ((ext
          '("m3u" "pls"                 ; incorporate playlist ext
            "flac" "m4a" "m4b"
            "aiff" "aac" "opus" "mkv"
            "ogv" "oga" "ogg" "mp3"  "mp4" "webm" "wav")))
      (concat
       "\\."
       (regexp-opt ext)
       "$")))
  "Media Extensions.")

(defconst  emacspeak-playlist-pattern
  (eval-when-compile
    (concat
     (regexp-opt
      (list ".m3u" ".asx" ".pls"  ".ram"))
     "$"))
  "Playlist pattern.")

;;; Executable Variable names:
;; emacspeak-<prog> as far as possible

;; amixer
(defconst emacspeak-amixer  (executable-find "amixer") "Amixer program")

;; wpctl:
(defconst emacspeak-wpctl (executable-find "wpctl") "wpctl executable")

;; curl:
(defconst emacspeak-curl (executable-find "curl") "Curl.")

;; git:
(defconst emacspeak-git (executable-find "git" "Git Executable"))

;; mpv:
(defconst emacspeak-mpv (executable-find "mpv") "MPV executable")
;; mplayer:
(defconst emacspeak-mplayer (executable-find "mplayer") "mplayer executable")
;; xsltproc
(defconst emacspeak-xslt (executable-find "xsltproc") "xslt engine")

;; sox, soxi and play

(defconst sox-play (executable-find "play") "Location of play")

(defconst sox-sox (executable-find "sox") "Location of sox")

(defconst sox-soxi (executable-find "soxi") "Location of soxi")

;; youtube-dl
(defconst emacspeak-ytdl (executable-find "youtube-dl") "Youtube DL Executable")

;;   xslt Environment:
(defsubst emacspeak-xslt-get (style)
  "Return  stylesheet path."
  (expand-file-name style emacspeak-xslt-directory))

(defconst emacspeak-opml-xsl
  (eval-when-compile  (emacspeak-xslt-get "opml.xsl"))
  "XSL stylesheet used for viewing OPML  Feeds.")

(defconst emacspeak-rss-xsl
  (eval-when-compile  (emacspeak-xslt-get "rss.xsl"))
  "XSL stylesheet used for viewing RSS Feeds.")

(defconst emacspeak-atom-xsl
  (eval-when-compile  (emacspeak-xslt-get "atom.xsl"))
  "XSL stylesheet used for viewing Atom Feeds.")

;;; Git Revision:
(defun emacspeak-get-revision ()
  "Get SHA checksum of current revision that is suitable for spoken output."
  (let ((default-directory emacspeak-directory))
    (if (and emacspeak-git
             (file-exists-p (expand-file-name ".git" emacspeak-directory)))
        (propertize
         (substring
          (shell-command-to-string "git show -s --pretty=format:%h
  HEAD ")
          0 7)
         'personality 'acss-s4-r6)
      "")))

(defconst emacspeak-git-revision
  (emacspeak-get-revision)
  "Git Revision")

;;; Pull in core libraries:
(mapc
 #'require
 '(
   dtk-speak voice-setup voice-defs
   emacspeak-pronounce emacspeak-keymap emacspeak-speak emacspeak-sounds))

;;;  Interactive Check Implementation Explained:

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

;;;; Design:
;; Advice on funcall-interactively stores the name of the
;; interactive command being run.
;; The defadvice macro  itself has a defadvice  to generate a locally bound
;; predicate that ensures that ems-interactive-p is only called from
;; within emacspeak advice forms.
;; Thus, ems-interactive-p is reserved for use within Emacspeak advice.
;;; Implementation: Interactive Check:

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

;;; defun: ems--fastload:

;; Internal function  used to efficiently load files.

(defun ems--fastload (file)
  "Load file efficiently."
  (let ((file-name-handler-alist nil)
        (load-source-file-function nil))
    (load file)))

(provide  'emacspeak-preamble)
