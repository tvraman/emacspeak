;;; emacspeak-info.el --- Speech enable Info -- Emacs' online documentation viewer  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable Emacs Info Reader.
;;; Keywords:emacspeak, audio interface to emacs
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4558 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2021, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;{{{ Introduction:

;;; Commentary:

;;; This module speech-enables the Emacs Info Reader.
;;; Code:

;;}}}
;;{{{ requires

(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'info)

;;}}}
;;{{{  Voices

(voice-setup-add-map
 '(
   (Info-quoted voice-lighten)
   (info-index-match 'voice-animate)
   (info-title-1 voice-bolden-extra)
   (info-title-2 voice-bolden-medium)
   (info-title-3 voice-bolden)
   (info-title-4 voice-lighten)
   (info-header-node voice-smoothen)
   (info-header-xref voice-brighten)
   (info-node voice-monotone-extra)
   (info-xref voice-animate-extra)
   (info-menu-star voice-brighten)
   (info-menu-header voice-bolden)
   (info-xref-visited voice-animate-medium)))

;;}}}
;;{{{ Module Prefixes 

'(
  ein:byte
  ein:debug
  ein:dev
  ein:file
  ein:header
  ein:ipdb
  ein:ipynb
  ein:jupyter
  ein:jupyterhub
  ein:log
  ein:login
  ein:markdown
  ein:notebook
  ein:notebooklist
  ein:pager
  ein:process
  ein:python
  ein:pytools
  ein:run
  ein:shared
  ein:stop
  ein:tb
  ein:traceback
  ein:version
  ein:worksheet
  )
;;}}}
;;{{{ advice

(defcustom  emacspeak-info-select-node-speak-chunk 'node
"Specifies how much of the selected node gets spoken.
Possible values are:
screenfull  -- speak the displayed screen
node -- speak the entire node."
  :type '(menu-choice
          (const :tag "First screenfull" screenfull)
          (const :tag "Entire node" node))
  :group 'emacspeak-info)

(defun emacspeak-info-speak-current-window ()
  "Speak current window in info buffer."
  (let ((start  (point))
        (window (get-buffer-window (current-buffer))))
    (save-excursion
      (forward-line (window-height window))
      (emacspeak-speak-region start (point)))))

(defun emacspeak-info-visit-node()
  "Apply requested action upon visiting a node."
  (cl-declare (special emacspeak-info-select-node-speak-chunk))
  (emacspeak-auditory-icon 'open-object)
  (cond
   ((eq emacspeak-info-select-node-speak-chunk 'screenfull)
    (emacspeak-info-speak-current-window))
   ((eq emacspeak-info-select-node-speak-chunk 'node)
    (emacspeak-speak-buffer))
   (t (emacspeak-speak-line))))

(cl-loop
 for f in
 '(info info-display-manual Info-select-node
        Info-follow-reference Info-goto-node info-emacs-manual
        Info-top-node Info-menu-last-node  Info-final-node Info-up
        Info-goto-emacs-key-command-node Info-goto-emacs-command-node
        Info-history Info-virtual-index Info-directory Info-help
        Info-nth-menu-item
        Info-menu Info-follow-nearest-node
        Info-history-back Info-history-forward
        Info-backward-node Info-forward-node
        Info-next Info-prev)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     " Speak the selected node based on setting of
emacspeak-info-select-node-speak-chunk"
     (when (ems-interactive-p) (emacspeak-info-visit-node)))))

(defadvice Info-search (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'search-hit)
    (emacspeak-speak-line)))

(defadvice Info-scroll-up (after emacspeak pre act comp)
  "Speak the screenful."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (let ((start  (point))
          (window (get-buffer-window (current-buffer))))
      (save-excursion
        (forward-line (window-height window))
        (emacspeak-speak-region start (point))))))

(defadvice Info-scroll-down (after emacspeak pre act comp)
  "Speak the screenful."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'scroll)
    (let ((start  (point))
          (window (get-buffer-window (current-buffer))))
      (save-excursion
        (forward-line (window-height window))
        (emacspeak-speak-region start (point))))))

(defadvice Info-exit (after emacspeak pre act comp)
  "Play an auditory icon to close info,
and then cue the next selected buffer."
  (when (ems-interactive-p)
    (dtk-stop)
    (emacspeak-auditory-icon 'close-object)
    (emacspeak-speak-mode-line)))

(defadvice Info-next-reference (after emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

(defadvice Info-prev-reference (after emacspeak pre act comp)
  "Speak the line. "
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'large-movement)
    (emacspeak-speak-line)))

;;;###autoload
(defun emacspeak-info-wizard (node-spec)
  "Read a node spec from the minibuffer and launch
Info-goto-node.
See documentation for command `Info-goto-node' for details on
node-spec."
  (interactive
   (list
    (let ((completion-ignore-case t)
          (f nil)
          (n nil))
      (info-initialize)
      (setq f (completing-read "File: " (info--manual-names nil) nil t))
      (setq n (completing-read "Node: " (Info-build-node-completions f)))
      (format "(%s)%s" f n))))
  (Info-goto-node node-spec)
  (emacspeak-info-visit-node))

;;}}}
;;{{{ Info: Section navigation
;;; Use property info-title-* to move across section titles.
(defvar emacspeak-info--title-faces
  '(info-title-1 info-title-2 info-title-3 info-title-4 info-menu-header)
  "Faces that identify section titles.")

(defun emacspeak-info-next-section ()
  "Move forward to next section in this node."
  (interactive)
  (let ((target nil))
    (save-excursion
      (while (and (null target)
                  (not (eobp)))
        (goto-char (next-single-property-change (point)  'face nil (point-max)))
        (when (memq (get-text-property (point) 'face) emacspeak-info--title-faces)
          (setq target (point)))))
    (cond
     (target
      (goto-char target)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))
     (t (message "No more sections in this node")))))

(defun emacspeak-info-previous-section ()
  "Move backward to previous section in this node."
  (interactive)
  (let ((target nil))
    (save-excursion
      (while (and (null target)
                  (not (bobp)))
        (goto-char (previous-single-property-change (point)  'face nil (point-min)))
        (when (memq (get-text-property (point) 'face) emacspeak-info--title-faces)
          (setq target (line-beginning-position)))))
    (cond
     (target
      (goto-char target)
      (emacspeak-speak-line)
      (emacspeak-auditory-icon 'large-movement))
     (t (message "No previous section in   this node")))))

;;}}}
;;{{{ Speak header line if hidden

(defun emacspeak-info-speak-header ()
  "Speak info header line."
  (interactive)
  (cl-declare (special Info-use-header-line
                       Info-header-line))
  (cond
   ((and (boundp 'Info-use-header-line)
         (boundp 'Info-header-line)
         Info-header-line)
    (dtk-speak Info-header-line))
   (t (save-excursion
        (goto-char (point-min))
        (emacspeak-speak-line)))))

;;}}}
;;{{{ keymaps

(cl-declaim (special Info-mode-map))
(define-key Info-mode-map "T" 'emacspeak-info-speak-header)
(define-key Info-mode-map "'" 'emacspeak-speak-rest-of-buffer)
(define-key Info-mode-map "\M-n" 'emacspeak-info-next-section)
(define-key Info-mode-map "\M-p" 'emacspeak-info-previous-section)

;;}}}
(provide  'emacspeak-info)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
