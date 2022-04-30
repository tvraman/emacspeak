;;; emacspeak-ruby.el --- Speech enable Ruby Mode  -*- lexical-binding: t; -*- 
;;
;; $Author: tv.raman.tv $ 
;; DescriptionEmacspeak extensions for Ruby mode
;; Keywords:emacspeak, audio interface to emacs Ruby
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}

;;{{{  Introduction:

;;; Commentary:

;; Provide additional advice to Ruby mode 

;;; Code:

;;}}}
;;{{{ required modules 

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
;;}}}
;;{{{ Advice navigation:

(cl-loop for command   in
         '(
           ruby-mark-defun
           ruby-beginning-of-defun 
           ruby-end-of-defun 
           ruby-beginning-of-block 
           ruby-end-of-block 
           ruby-forward-sexp
           ruby-backward-sexp
           )
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-speak-line)
               (emacspeak-auditory-icon 'paragraph)))))

;;}}}
;;{{{ Advice insertion and electric:

(defadvice ruby-insert-end (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (save-excursion
      (ruby-beginning-of-block)
      (emacspeak-speak-line))))

(defadvice ruby-reindent-then-newline-and-indent (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice ruby-indent-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice ruby-indent-exp (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-auditory-icon 'fill-object)))
(unless (and (boundp 'post-self-insert-hook)
             post-self-insert-hook
             (memq 'emacspeak-post-self-insert-hook post-self-insert-hook))
  (defadvice ruby-electric-brace (after emacspeak pre act comp)
    "Speak what you inserted.
Cue electric insertion with a tone."
    (when (ems-interactive-p)
      (let ((emacspeak-speak-messages nil))
        (emacspeak-speak-this-char last-input-event)
        (dtk-tone 800 100 t)))))

;;}}}
;;{{{ Advice inferior ruby:
(cl-loop for command in
         '(
           ruby-run
           switch-to-ruby
           ruby-send-region-and-go
           ruby-send-block-and-go
           ruby-send-definition-and-go
           )
         do
         (eval
          `(defadvice ,command (after emacspeak pre act comp)
             "speak."
             (when (ems-interactive-p)
               (emacspeak-auditory-icon 'select-object)
               (emacspeak-speak-line)))))

;;}}}

(provide  'emacspeak-ruby)
;;{{{  emacs local variables 

;; local variables:
;; folded-file: t
;; end: 

;;}}}
