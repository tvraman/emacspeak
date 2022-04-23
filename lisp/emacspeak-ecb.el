;;; emacspeak-ecb.el --- speech-enable Emacs Class Browser  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Emacspeak module for speech-enabling Emacs
;; Class Browser
;; Keywords: Emacspeak, ecb
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; $Date: 2007-09-01 15:30:13 -0700 (Sat, 01 Sep 2007) $ |
;;  $Revision: 4074 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:

;; Copyright (C) 1995 -- 2021, T. V. Raman
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{{{  Introduction:

;;; Commentary:
;; The ECB is an Emacs Class Browser.
;; This module speech-enables ECB
;;; Code:

;;}}}
;;{{{ required modules

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;}}}
;;{{{ Forward Declarations:

(declare-function tree-node->expandable "tree-buffer" (cl-x))
(declare-function tree-node->expanded "tree-buffer" (cl-x))
(declare-function ecb-goto-window-methods "ecb-method-browser" nil)
(declare-function ecb-goto-window-directories "ecb-file-browser" nil)
(declare-function ecb-goto-window-history "ecb-file-browser" nil)
(declare-function ecb-goto-window-sources "ecb-file-browser" nil)

;;}}}
;;{{{  advice interactive commands

(defadvice ecb-activate (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice ecb-cancel-dialog (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice ecb-show-help (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'help)
    (emacspeak-speak-mode-line)))

(cl-loop
 for f in 
 '(
   ecb-nav-goto-next
   ecb-nav-goto-previous
   ecb-goto-window-compilation
   ecb-goto-window-directories 
   ecb-goto-window-sources 
   ecb-goto-window-methods 
   ecb-goto-window-history 
   ecb-goto-window-edit1 
   ecb-goto-window-edit2)
 do
 (eval 
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-speak-line)
       (emacspeak-auditory-icon 'select-object)))))

(defadvice ecb-select-ecb-frame (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-speak-mode-line)
    (emacspeak-auditory-icon 'select-object)))

;;}}}
;;{{{  inform tree browser about emacspeak

;; define emacspeak versions of these special tree search
;; commands
;; need these to get ECB working outside X

(defun emacspeak-ecb-tree-backspace ()
  "Back up during incremental search in tree buffers."
  (interactive)
  (cl-declare (special tree-buffer-incr-searchpattern))
  ;; reduce by one from the end
  (setq tree-buffer-incr-searchpattern
        (substring tree-buffer-incr-searchpattern
                   0
                   (max 0 (1- (length
                               tree-buffer-incr-searchpattern)))))
  (dtk-speak  tree-buffer-incr-searchpattern)
  (emacspeak-auditory-icon 'delete-object))

(defun emacspeak-ecb-tree-clear ()
  "Clear search pattern during incremental search in tree buffers."
  (interactive)
  (cl-declare (special tree-buffer-incr-searchpattern))  
  (setq tree-buffer-incr-searchpattern "")
  (dtk-speak "Cleared search pattern."))

(defadvice tree-buffer-create (after emacspeak pre act comp)
  "Fixes up keybindings so incremental tree search is
available."
  (let ((incr-search (ad-get-arg 10)))
    (when incr-search
      (substitute-key-definition 'emacspeak-self-insert-command
                                 'tree-buffer-incremental-node-search
                                 tree-buffer-key-map
                                 global-map))
    (define-key tree-buffer-key-map "\d"
      'emacspeak-ecb-tree-backspace)
    (define-key tree-buffer-key-map '[delete]
      'emacspeak-ecb-tree-backspace)
    (define-key tree-buffer-key-map '[home]
      'emacspeak-ecb-tree-clear)
    ))

(defadvice tree-buffer-incremental-node-search 
    (around emacspeak pre act comp)
  "Track search and provide appropriate auditory feedback."
  
  (cond
   ((ems-interactive-p)
    (let ((start (point))
          (beg nil)
          (end nil))
      ad-do-it
      (cond
       ((not (=  start (point)))
        (let ((emacspeak-speak-messages nil)
              (case-fold-search t))
          (save-excursion
            (beginning-of-line)
            (setq beg (point))
            (backward-char 1)
            (search-forward tree-buffer-incr-searchpattern)
            (setq end (point))
            (with-silent-modifications
              (ems-set-personality-temporarily
               beg end   voice-bolden
               (emacspeak-speak-line)))
            (emacspeak-auditory-icon 'search-hit))))
       (t (emacspeak-auditory-icon 'search-miss)))))
   (t ad-do-it))
  ad-return-value)

(defadvice tree-buffer-select (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))
(defun tree-node-is-expanded (node)
  "Check if node is expanded."
  (or (not (tree-node->expandable node))
      (tree-node->expanded node)))

(defadvice tree-node-toggle-expanded (after emacspeak pre
                                            act comp)
  "speak."
  (when (ems-interactive-p)
    (let ((node (ad-get-arg 0))) ;; note that logic is reversed
      (cond
       ((tree-node-is-expanded node)
        (emacspeak-auditory-icon 'open-object))
       (t (emacspeak-auditory-icon 'close-object))))))

(defadvice tree-buffer-update (after emacspeak pre act comp)
  "Provide context speech feedback."
  (when (ems-interactive-p)
    (emacspeak-speak-line)))

(defadvice tree-buffer-nolog-message (after emacspeak pre
                                            act comp)
  "Speak the message."
  (dtk-speak ad-return-value))

(defadvice tree-buffer-arrow-pressed (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

(defadvice tree-buffer-tab-pressed (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

(defadvice tree-buffer-return-pressed (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'button)
    (emacspeak-speak-line)))

(defadvice tree-buffer-show-menu-keyboard (around emacspeak pre
                                                  act comp)
  "When on the console, always use TMM."
  (cond
   ((and (ems-interactive-p)
         (not (display-graphic-p)))
    (tree-buffer-show-menu-keyboard 'use-tmm))
   (t ad-do-it)))

;;}}}
;;{{{ commands to speak ECB windows without  moving

(defun emacspeak-ecb-speak-window-methods ()
  "Speak contents of methods window."
  (interactive)
  (save-excursion
    (save-window-excursion
      (ecb-goto-window-methods)
      (emacspeak-speak-buffer))))

(defun emacspeak-ecb-speak-window-directories ()
  "Speak contents of directories window."
  (interactive)
  (save-excursion
    (save-window-excursion
      (ecb-goto-window-directories)
      (emacspeak-speak-buffer))))

(defun emacspeak-ecb-speak-window-history ()
  "Speak contents of history window."
  (interactive)
  (save-excursion
    (save-window-excursion
      (ecb-goto-window-history)
      (emacspeak-speak-buffer))))

(defun emacspeak-ecb-speak-window-sources ()
  "Speak contents of sources window."
  (interactive)
  (save-excursion
    (save-window-excursion
      (ecb-goto-window-sources)
      (emacspeak-speak-buffer))))

;;}}}
(provide 'emacspeak-ecb)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
