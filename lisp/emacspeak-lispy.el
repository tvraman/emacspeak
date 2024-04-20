;;; emacspeak-lispy.el --- Speech-enable LISPY  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable LISPY An Emacs Interface to lispy
;; Keywords: Emacspeak,  Audio Desktop lispy
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;; 

;;;   Copyright:

;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;; MERCHANTABILITY or FITNLISPY FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; LISPY == smart Navigation Of Lisp code This module speech-enables
;; lispy.
;; @subsection Overview
;; Lispy editing keeps delimiters
;; balanced and Lispy navigators reliably place point on either the
;; opening or closing delimiter of the current s-expression. Emacspeak
;; leverages this fact in the type of spoken feedback that is
;; produced. All navigation commands produce the following:
;; @itemize
;; @item Speak the current s-expression when at the front of a sexp.
;; @item Speak the current line with option
;; @code{emacspeak-show-point} turned on when at the end of an
;; s-expression. 
;;  @item Produce auditory icon @code{left} or
;; @code{right} to indicate point being at the beginning or end of
;; current line. 
;; @item Indicate with an auditory icon if point did
;; not move.
;;   @end itemize

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'lispy "lispy" 'no-error)

;;;  Map Faces:

(voice-setup-add-map
 '(
   (lispy-command-name-face voice-bolden)
   (lispy-cursor-face voice-animate)
   (lispy-face-hint voice-smoothen)
   (lispy-face-key-nosel voice-monotone-extra)
   (lispy-face-key-sel voice-brighten)
   (lispy-face-opt-nosel voice-monotone-extra)
   (lispy-face-opt-sel voice-lighten)
   (lispy-face-req-nosel voice-monotone-extra)
   (lispy-face-req-sel voice-brighten-extra)
   (lispy-face-rst-nosel voice-monotone-extra)
   (lispy-face-rst-sel voice-lighten-extra)
   (lispy-test-face voice-annotate)))

;;;  Setup:

(defun emacspeak-lispy-setup ()
  "Setup emacspeak for use with lispy"
  (cl-declare (special lispy-mode-map))
  (when (bound-and-true-p lispy-mode-map)
    (define-key lispy-mode-map (kbd "C-e") 'emacspeak-keymap)))

(emacspeak-lispy-setup)

;;;  Advice Navigation:

(cl-loop ;;; Navigators:
 for f in
 '(
   lispy-goto-symbol lispy-splice lispy-view
   lispy-stringify lispy-ace-paren lispy-ace-symbol lispy-teleport
   lispy-ace-char lispy-ace-subword lispy-move-up lispy-move-down lispy-undo
   lispy-right-nostring lispy-left lispy-right lispy-up lispy-down lispy-back
   lispy-different lispy-backward lispy-forward lispy-flow
   lispy-to-defun lispy-beginning-of-defun)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "speak.
Speak sexp when at the beginning of a sexp.
Speak line if at end of sexp.
Indicate  no movement if we did not move."
     (cond
      ((ems-interactive-p)
       (let ((emacspeak-show-point t)
             (orig (point)))
         ad-do-it
         (cond
          ((eq orig (point))
           (dtk-notify "Did not move")
           (emacspeak-icon 'tick-tick))
          ((= ?\) (char-syntax (preceding-char)))
           (emacspeak-icon 'select-object)
           (emacspeak-speak-line))
          (t (emacspeak-icon 'select-object)
             (emacspeak-speak-sexp)))))
      (t ad-do-it))
     ad-return-value)))

(defadvice lispy-move-beginning-of-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-icon 'left)))

(defadvice lispy-move-beginning-of-line (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-icon 'right)))

;;; Advice Insertions:

(defadvice lispy-clone (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-sexp)
    (emacspeak-icon 'yank-object)))

(defadvice lispy-comment (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'select-object)
    (cond
     ((use-region-p)(emacspeak-speak-region (region-beginning) (region-end)))
     (t (emacspeak-speak-line)))))

(defadvice lispy-backtick (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))

(defadvice lispy-tick (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (cond
     ((region-active-p)
      (emacspeak-speak-region (region-beginning) (region-end)))
     (t (emacspeak-speak-line)))))

(cl-loop
 for f in
 '(lispy-at lispy-colon lispy-hash lispy-hat)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-this-char (preceding-char))))))

(cl-loop
 for f in
 '(lispy-parens lispy-braces lispy-brackets)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'item)
       (save-excursion
         (forward-char 1)
         (emacspeak-speak-sexp))))))

;;;  Slurp and barf:

(cl-loop
 for f in
 '(
   lispy-barf lispy-slurp lispy-join lispy-split
   lispy-quotes lispy-alt-multiline
   lispy-out-forward-newline lispy-parens-down lispy-meta-return)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak line with show-point turned on."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-icon 'select-object)
         (emacspeak-speak-line))))))

;;; Advice Marking:

(cl-loop
 for f in
 '(lispy-mark-list lispy-mark)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'mark-object)
       (emacspeak-speak-region (region-beginning) (region-end))))))

(defadvice lispy-mark-symbol (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'mark-object)
    (emacspeak-speak-region  (region-beginning) (region-end))))

;;; Advice WhiteSpace Manipulation:
(defadvice lispy-fill (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'fill-object)
    (emacspeak-speak-line)))

(cl-loop
 for f in
 '(lispy-newline-and-indent lispy-newline-and-indent-plain)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line))))))

(defadvice lispy-tab (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'fill-object)
    (when (buffer-modified-p) (emacspeak-icon 'modified-object))
    (emacspeak-speak-line)))

;;; Advice Kill/Yank:
(defadvice lispy-new-copy (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'mark-object)
    (message "region containing %s chars copied to kill ring "
             (length (current-kill 0)))))

(cl-loop
 for f in
 '(lispy-kill lispy-kill-word lispy-backward-kill-word
              lispy-kill-sentence lispy-kill-at-point)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'delete-object)
       (dtk-speak (current-kill 0 nil))))))

(defadvice lispy-yank (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'yank-object)
    (emacspeak-speak-region (region-beginning) (region-end))))

(defadvice lispy-delete-backward(around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (emacspeak-icon 'delete-object)
    (emacspeak-speak-this-char (preceding-char))
    ad-do-it)
   (t ad-do-it)))

(defadvice lispy-delete (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (dtk-tone-deletion)
    (emacspeak-speak-char t)
    ad-do-it)
   (t ad-do-it)))

;;; Advice Help:

(defadvice lispy-describe-inline (after emacspeak pre act comp)
  "speak."
  (when
      (and
       (ems-interactive-p)
       (buffer-live-p (get-buffer "*lispy-help*"))
       (window-live-p (get-buffer-window "*lispy-help*")))
    (with-current-buffer  "*lispy-help*"
      (emacspeak-icon 'help)
      (emacspeak-speak-buffer))))

(defadvice lispy--show (before emacspeak   pre act comp)
  "speak."
  (emacspeak-icon 'help)
  (dtk-speak (ad-get-arg 0)))

;;; Advice Outliner:

(defadvice lispy-narrow (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-icon 'mark-object)
    (message "Narrowed editing region to %s lines"
             (count-lines (region-beginning)
                          (region-end)))))

(defadvice lispy-widen (after emacspeak pre act comp)
  "Announce yourself."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (message "You can now edit the entire buffer ")))

(cl-loop
 for f in
 '(lispy-outline-next lispy-outline-prev lispy-shifttab)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line))))))

(provide 'emacspeak-lispy)
;;;  end of file

