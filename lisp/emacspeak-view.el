;;; emacspeak-view.el --- Speech enable View mode - -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; DescriptionEmacspeak extensions for view
;; Keywords:emacspeak, audio interface to emacs, view-mode
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
;; Copyright (c) 1996 by T. V. Raman
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
;; Provide additional advice to view-mode
;;; Code:

;;;  requires
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;   Setup view mode to work with emacspeak

;; restore emacspeak keybindings:
(cl-declaim (special emacspeak-prefix))
  
(add-hook
 'view-mode-hook
 #'(lambda ()
     (local-unset-key emacspeak-prefix)
     (emacspeak-speak-load-directory-settings)
     (outline-minor-mode 1))
 'at-end)

;;;  Advise additional interactive commands:

(defadvice view-mode (after emacspeak pre act comp)
  "Announce what happened"
  (cl-declare (special view-mode-map))
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (if view-mode
        (message "Entered view mode Press %s to exit"
                 (key-description
                  (where-is-internal 'View-exit view-mode-map 'firstonly)))
      (message "Exited view mode"))))

(cl-loop
 for f in
 '(
   View-exit-and-edit View-kill-and-leave View-quit-all View-quit)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'close-object)
       (emacspeak-speak-mode-line)))))

(cl-loop
 for f in
 '(
   view-buffer view-buffer-other-frame view-buffer-other-window
   view-emacs-FAQ view-emacs-debugging ^ view-emacs-problems
   view-emacs-todo view-external-packages
   view-file-other-frame view-file-other-window
   view-hello-file view-lossage ) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak"
     (when (ems-interactive-p)
       (emacspeak-icon 'open-object)
       (emacspeak-speak-mode-line)))))

(cl-loop
 for f in
 '(
   View-search-regexp-forward View-search-regexp-backward
   View-search-last-regexp-backward View-search-last-regexp-forward
   ) do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (let ((emacspeak-show-point t))
         (emacspeak-speak-line))
       (emacspeak-icon 'search-hit)))))

(cl-loop
 for f in
 '(
   View-scroll-half-page-backward View-scroll-half-page-forward
View-scroll-line-backward View-scroll-line-forward
View-scroll-lines-forward-set-scroll-size View-scroll-one-more-line
View-scroll-page-backward view-scroll-page-forward 
View-scroll-page-backward-set-page-size View-scroll-page-forward-set-page-size
) do
 `(eval
   (defadvice ,f (after emacspeak pre act comp)
     "speak"
     (when (ems-interactive-p)
       (emacspeak-icon 'scroll)
       (emacspeak-speak-windowful)))))

(defadvice View-back-to-mark (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (let ((emacspeak-show-point t))
      (emacspeak-speak-line))))

(defadvice View-goto-line (after emacspeak pre act comp)
  "Speak"
  (when (ems-interactive-p)
    (let ((line-number
           (format "line %s"
                   (ad-get-arg 0))))
      (put-text-property 0 (length line-number)
                         'personality voice-annotate line-number)
      (emacspeak-icon 'large-movement)
      (dtk-speak
       (concat line-number (ems--this-line))))))

(defadvice View-scroll-to-buffer-end (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-speak-line)
    (emacspeak-icon 'large-movement)))

(defadvice View-goto-percent (after emacspeak pre act comp)
  "speak"
  (when (ems-interactive-p)
    (emacspeak-icon 'scroll)
    (dtk-speak (emacspeak-get-window-contents))))

;;;  bind convenience keys

(defun emacspeak-view-setup-keys()
  "Setup emacspeak convenience keys"
  (cl-declare (special view-mode-map))
  (cl-loop
   for  b in
   '(
     ("," 'emacspeak-speak-current-window)
     ("C-j" 'emacspeak-hide-speak-block-sans-prefix)
     ("M-d" 'emacspeak-pronounce-dispatch)
     ("M-n" 'outline-next-visible-heading)
     ("M-p" 'outline-previous-visible-heading)
     ("SPC" 'scroll-up)
     ("[" 'backward-page)
     ("]" 'forward-page)
     ("b" backward-word)
     ("c" 'emacspeak-speak-char)
     ("d" 'scroll-down)
     ("f" forward-word)
     ("h" left-char)
     ("j" next-line)
     ("k" previous-line)
     ("l" right-char)
     ("w" emacspeak-speak-word)
     )
   do
   (emacspeak-keymap-update view-mode-map b))
  (cl-loop
   for i from 0 to 9
   do
   (define-key view-mode-map (format "%s" i) 'dtk-set-predefined-rate)))

(emacspeak-view-setup-keys)

(provide  'emacspeak-view)
