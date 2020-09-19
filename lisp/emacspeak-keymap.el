;;; emacspeak-keymap.el --- Setup  keymaps and keybindings provided by Emacspeak  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description:  Module for setting up emacspeak keybindings
;;; Keywords: Emacspeak
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2008-06-21 10:50:41 -0700 (Sat, 21 Jun 2008) $ |
;;;  $Revision: 4544 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2018, T. V. Raman
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module defines the emacspeak keybindings.
;;; Note that the <fn> key found on laptops is denoted <XF86WakeUp>

;;; Code:

;;}}}
;;{{{ requires

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'wid-edit)

;;}}}
;;{{{ Custom Widget Types:

;;;###autoload
(defun emacspeak-keymap-command-p (s)
  "Check if `s' is suitable to be bound to a key."
  (or (commandp s) (keymapp s)))

;;;###autoload
(defun emacspeak-keymap-update (keymap binding)
  "Update keymap with specified binding."
  (define-key keymap  (kbd (cl-first binding)) (cl-second binding)))

(defun emacspeak-keymap-bindings-update (keymap bindings)
  "Update keymap with specified list of bindings."
  (cl-loop
   for binding in bindings
   do
   (define-key keymap (kbd (cl-first binding)) (cl-second binding)))) 

(define-widget 'ems-interactive-command 'restricted-sexp
  "An interactive command  or keymap that can be bound to a key."
  :completions
  (apply-partially #'completion-table-with-predicate
                   obarray 'emacspeak-keymap-command-p 'strict)
  :prompt-value 'widget-field-prompt-value
  :prompt-internal 'widget-symbol-prompt-internal
  :prompt-match 'emacspeak-keymap-command-p
  :prompt-history 'widget-function-prompt-value-history
  :action 'widget-field-action
  :match-alternatives '(emacspeak-keymap-command-p)
  :validate (lambda (widget)
              (unless (emacspeak-keymap-command-p (widget-value widget))
                (widget-put widget :error
                            (format "Invalid interactive command : %S"
                                    (widget-value widget)))
                widget))
  :value 'ignore
  :tag "Interactive Command")

;;}}}
;;{{{  variables:

(defvar emacspeak-prefix (kbd "C-e")
  "Default prefix key used for emacspeak. ")


(defvar emacspeak-keymap nil
  "Primary keymap used by emacspeak. ")

(defvar emacspeak-dtk-submap nil
  "Submap used for DTK commands. ")

(defvar emacspeak-table-submap nil
  "Submap used for table  commands. ")

;;}}}
;;{{{   Binding keymap and submap

(define-prefix-command 'emacspeak-prefix-command 'emacspeak-keymap)
(define-prefix-command  'emacspeak-dtk-submap-command 'emacspeak-dtk-submap)
(define-prefix-command  'emacspeak-table-submap-command 'emacspeak-table-submap)

(global-set-key emacspeak-prefix 'emacspeak-prefix-command)
;;; One-finger use on laptop:
(global-set-key (kbd "<XF86WakeUp>")  'emacspeak-prefix-command)
(define-key emacspeak-keymap "d"  'emacspeak-dtk-submap-command)
(define-key emacspeak-keymap (kbd "C-t")  'emacspeak-table-submap-command)

;;; fix what we just broke:-)
(define-key emacspeak-keymap "e" 'end-of-line)
(define-key emacspeak-keymap (kbd "C-e") 'end-of-line)

;;}}}
;;{{{  The Emacspeak key  bindings.

;;; help map additions:

(cl-loop for binding in
         '(
           (":" describe-help-keys)
           ("B" customize-browse)
           ("G" customize-group)
           ("M-m" describe-minor-mode-from-indicator)
           ("T" emacspeak-view-notifications)
           ("M-F" find-function-at-point)
           ("M-V" find-variable-at-point)
           ("M-f" find-function)
           ("M-k" find-function-on-key)
           ("M-v" find-variable)
           ("V" customize-variable)
           ("C-e"   emacspeak-describe-emacspeak)
           ("C-j" finder-commentary)
           ("C-l" emacspeak-learn-emacs-mode)
           ("C-m" man)
           ("C-s" emacspeak-wizards-customize-saved)
           ("C-r" info-display-manual)
           ("SPC" customize-group)
           ("TAB" emacspeak-info-wizard)
           ("C-<tab>" emacs-index-search)
           ("M" emacspeak-speak-popup-messages)
           ("'" describe-text-properties)
           ("\"" voice-setup-list-voices)
           ("," emacspeak-wizards-color-at-point)
           ("=" emacspeak-wizards-swap-fg-and-bg)
           ("\\" emacspeak-wizards-color-diff-at-point)
           ("/" describe-face)
           ("C-v" voice-setup-describe-personality)
           ("\;" describe-font)
           ("p" list-packages)
           )
         do
         (emacspeak-keymap-update help-map binding))

;;; emacspeak-keymap bindings:
(cl-loop
 for binding in
 '(
   ("C-a" emacspeak-toggle-auditory-icons)
   ("M-f" emacspeak-frame-label-or-switch-to-labelled-frame)
   ("!" emacspeak-speak-run-shell-command)
   ("#" emacspeak-gridtext)
   ("$" emacspeak-shell-command)
   ("%" emacspeak-speak-current-percentage)
   ("&" emacspeak-wizards-shell-command-on-current-file)
   ("\"" emacspeak-speak-sexp)
   ("(" emacspeak-audio-setup)
   (")" emacspeak-sounds-select-theme)
   ("," emacspeak-speak-skim-buffer)
   ("." emacspeak-speak-current-field)
   ("/" emacspeak-speak-this-buffer-other-window-display)
   (":" emacspeak-m-player-shuffle)
   (";" emacspeak-multimedia)
   ("<" emacspeak-speak-previous-field)
   ("<(deletechar>" emacspeak-ssh-tts-restart)
   ("C-<left>" emacspeak-select-this-buffer-previous-display)
   ("C-<right>" emacspeak-select-this-buffer-next-display)
   ("<delete>" emacspeak-ssh-tts-restart)
   ("<down>" emacspeak-read-next-line)
   ("<f11>" emacspeak-wizards-shell-toggle)
   ("<f1>" emacspeak-learn-emacs-mode)
   ("<insert>" emacspeak-emergency-tts-restart)
   ("<left>" emacspeak-speak-this-buffer-previous-display)
   ("<right>" emacspeak-speak-this-buffer-next-display)
   ("<up>"  emacspeak-read-previous-line)
   ("=" emacspeak-speak-current-column)
   (">"  emacspeak-speak-next-field)
   ("?" emacspeak-websearch-dispatch)
   ("@" emacspeak-speak-message-at-time)
   ("A" emacspeak-appt-repeat-announcement)
   ("B" emacspeak-speak-buffer-interactively)
   ("C" emacspeak-customize)
   ("C-@" emacspeak-speak-current-mark)
   ("C-SPC" emacspeak-speak-current-mark)
   ("C-b" emacspeak-bookshare)
   ("C-c" emacspeak-clipboard-copy)
   ("C-d" emacspeak-toggle-show-point)
   ("M-i" emacspeak-table-display-table-in-region)
   ("C-i" emacspeak-open-info)
   ("C-j" emacspeak-hide-speak-block-sans-prefix)
   ("C-l" emacspeak-speak-line-number)
   ("M-C-l" emacspeak-speak-overlay-properties)
   ("C-m"  emacspeak-speak-continuously)
   ("C-n" emacspeak-speak-next-window)
   ("C-o" emacspeak-ocr)
   ("C-p" emacspeak-speak-previous-window)
   ("C-q" emacspeak-toggle-inaudible-or-comint-autospeak)
   ("C-s" tts-restart)
   ("C-u" emacspeak-feeds-browse)
   ("C-v" view-mode)
   ("C-w" emacspeak-speak-window-information)
   ("M-w" emacspeak-speak-which-function)
   ("C-y" emacspeak-clipboard-paste)
   ("DEL" cd-tool)
   ("I"  emacspeak-speak-show-active-network-interfaces)
   ("L" emacspeak-speak-line-interactively)
   ("M" emacspeak-speak-minor-mode-line)
   ("M-%" emacspeak-goto-percent)
   ("M-;" emacspeak-webutils-play-media-at-point)
   ("M-C-@" emacspeak-speak-spaces-at-point)
   ("M-C-SPC" emacspeak-speak-spaces-at-point)
   ("M-C-b" emacspeak-submit-bug)
   ("M-C-k" kill-emacs)
   ("M-a" emacspeak-set-auditory-icon-player)
   ("M-b" emacspeak-speak-other-buffer)
   ("M-c" emacspeak-copy-current-file)
   ("M-d" emacspeak-pronounce-dispatch)
   ("M-h" emacspeak-speak-hostname)
   ("M-l" emacspeak-link-current-file)
   ("M-m" emacspeak-toggle-mail-alert)
   ("M-q" voice-setup-toggle-silence-personality)
   ("M-r" emacspeak-remote-connect-to-server)
   ("M-s" emacspeak-symlink-current-file)
   ("M-t" emacspeak-tapestry-describe-tapestry)
   ("M-u" emacspeak-feeds-add-feed)
   ("M-p" emacspeak-show-property-at-point)
   ("M-v" emacspeak-show-style-at-point)
   ("M-x" emacspeak-wizards-execute-emacspeak-command)
   ("N" emacspeak-view-emacspeak-news)
   ("P" emacspeak-speak-paragraph-interactively)
   ("R" emacspeak-speak-rectangle)
   ("SPC" emacspeak-speak-header-line)
   ("T" emacspeak-view-emacspeak-tips)
   ("V" emacspeak-speak-version)
   ("W" emacspeak-tapestry-select-window-by-name)
   ("[" emacspeak-speak-page)
   ("'" emacspeak-pianobar)
   ("\\" emacspeak-toggle-speak-line-invert-filter)
   ("]" emacspeak-speak-page-interactively)
   ("^" emacspeak-filtertext)
   ("a" emacspeak-speak-message-again)
   ("b" emacspeak-speak-buffer)
   ("c" emacspeak-speak-char)
   ("f" emacspeak-speak-buffer-filename)
   ("g" emacspeak-epub)
   ("h" emacspeak-speak-help)
   ("i" emacspeak-tabulate-region)
   ("j" emacspeak-hide-or-expose-block)
   ("k" emacspeak-speak-current-kill)
   ("l" emacspeak-speak-line)
   ("m" emacspeak-speak-mode-line)
   ("n" emacspeak-speak-rest-of-buffer)
   ("o" emacspeak-toggle-comint-output-monitor)
   ("p" emacspeak-speak-paragraph)
   ("q" emacspeak-toggle-speak-messages)
   ("r" emacspeak-speak-region)
   ("s" dtk-stop)
   ("t" emacspeak-speak-time)
   ("u" emacspeak-url-template-fetch)
   ("v" view-register)
   ("w" emacspeak-speak-word)
   ("|" emacspeak-speak-line-set-column-filter)
   ("." dtk-notify-stop)
   )
 do
 (emacspeak-keymap-update emacspeak-keymap binding))

(dotimes (i 10)
  (define-key emacspeak-keymap   (format "%s" i)
    'emacspeak-speak-predefined-window))

(cl-loop
 for binding in
 '(
   ("," dtk-toggle-punctuation-mode)
   ("V" tts-speak-version)
   ("C" dtk-toggle-allcaps-beep)
   ("L" dtk-local-server)
   ("N" dtk-set-next-language)
   ("P" dtk-set-previous-language)
   ("R" dtk-reset-state)
   ("S" dtk-set-language)
   ("SPC" dtk-toggle-splitting-on-white-space)
   ("C-v" global-voice-lock-mode)
   ("v" voice-lock-mode)
   ("C-c" dtk-cloud)
   ("C-d" dectalk)
   ("C-e" espeak)
   ("C-m" dtk-set-chunk-separator-syntax)
   ("C-o" outloud)
   ("a" dtk-add-cleanup-pattern)
   ("c" dtk-toggle-capitalization)
   ("d" dtk-select-server)
   ("f" dtk-set-character-scale)
   ("i" emacspeak-toggle-audio-indentation)
   ("k" emacspeak-toggle-character-echo)
   ("l" emacspeak-toggle-line-echo)
   ("m"emacspeak-speak-set-mode-punctuations)
   ("n" dtk-toggle-speak-nonprinting-chars)
   ("o" dtk-toggle-strip-octals)
   ("p" dtk-set-punctuations)
   ("r" dtk-set-rate)
   ("s" dtk-toggle-split-caps)
   ("w" emacspeak-toggle-word-echo)
   ("z" emacspeak-zap-tts)
   ("C-n" dtk-notify-initialize)
   ("C-s" dtk-notify-shutdown))
 do
 (emacspeak-keymap-update emacspeak-dtk-submap binding))

(dotimes (i 10)
  (define-key emacspeak-dtk-submap
    (format "%s" i)   'dtk-set-predefined-speech-rate))

(cl-loop
 for binding in
 '(
   ("f" emacspeak-table-find-file)
   ("," emacspeak-table-find-csv-file)
   )
 do
 (emacspeak-keymap-update emacspeak-table-submap binding))

;;; Put these in the global map:
(global-set-key [(shift left)] 'switch-to-prev-buffer)
(global-set-key [(shift right)] 'switch-to-next-buffer)
(global-set-key [(control left)] 'emacspeak-previous-frame-or-buffer)
(global-set-key [(control right)] 'emacspeak-next-frame-or-buffer)
(global-set-key [(control down)] 'pop-to-mark-command)
(global-set-key [(control up)] 'emacspeak-mark-backward-mark)
(global-set-key [(shift up)] 'emacspeak-skip-blank-lines-backward)
(global-set-key [(shift down)] 'emacspeak-skip-blank-lines-forward)
(global-set-key [27 up]  'emacspeak-owindow-previous-line)
(global-set-key  [27 down]  'emacspeak-owindow-next-line)
(global-set-key  [27 prior]  'emacspeak-owindow-scroll-down)
(global-set-key  [27 next]  'emacspeak-owindow-scroll-up)
(global-set-key  [27 select]  'emacspeak-owindow-speak-line)
(define-key esc-map "\M-:" 'emacspeak-wizards-show-eval-result)

;;}}}
;;{{{ emacspeak under X windows

;;; Get hyper, alt and super like on the console:
(global-set-key (kbd "C-,") 'emacspeak-alt-keymap)
(global-set-key  (kbd "C-'") 'emacspeak-super-keymap)
(global-set-key  (kbd "C-.") 'emacspeak-super-keymap)
(global-set-key  (kbd "C-;") 'emacspeak-hyper-keymap)
;;; Our very own silence key on the console
(global-set-key '[silence] 'emacspeak-silence)
(global-set-key '[search] 'emacspeak-search)

;;}}}
;;{{{ Interactively switching the emacspeak-prefix

;;;###autoload
(defun emacspeak-keymap-choose-new-emacspeak-prefix (prefix-key)
  "Interactively select a new prefix key to use for all emacspeak
commands.  The default is to use `C-e'  This command
lets you switch the prefix to something else.  This is a useful thing
to do if you run emacspeak on a remote machine from inside a terminal
that is running inside a local emacspeak session.  You can have the
remote emacspeak use a different control key to give your fingers some
relief. Note: I've not used this in over 20 years."
  (interactive (list (read-key-sequence "Emacspeak Prefix: ")))
  (cl-declare (special emacspeak-prefix))
  (let ((current-use (lookup-key  global-map prefix-key)))
    (global-set-key prefix-key 'emacspeak-prefix-command)
    (unless (eq  current-use 'emacspeak-prefix-command)
      (global-set-key (concat prefix-key prefix-key) current-use)
      (message "Use %s %s to execute %s since %s is now the emacspeak prefix"
               prefix-key prefix-key current-use
               prefix-key))))

;;}}}
;;{{{ Create a personal keymap for c-e x

;;; Adding keys using custom:
(defvar  emacspeak-personal-keymap nil
  "Emacspeak personal keymap")

(define-prefix-command 'emacspeak-personal-keymap   'emacspeak-personal-keymap)

(defcustom emacspeak-personal-keys 
  '(
    ("0" emacspeak-wizards-shell-by-key) 
    ("1" emacspeak-wizards-shell-by-key) 
    ("2" emacspeak-wizards-shell-by-key) 
    ("3" emacspeak-wizards-shell-by-key) 
    ("4" emacspeak-wizards-shell-by-key) 
    ("5" emacspeak-wizards-shell-by-key) 
    ("6" emacspeak-wizards-shell-by-key) 
    ("7" emacspeak-wizards-shell-by-key) 
    ("8" emacspeak-wizards-shell-by-key) 
    ("9" emacspeak-wizards-shell-by-key) 
    ("." emacspeak-wizards-shell-directory-reset)
    ("," emacspeak-wizards-shell-directory-set)
    ("=" emacspeak-wizards-find-longest-line-in-region)
    ("a" emacspeak-wizards-alpha-vantage-quotes)
    ("b" battery)
    ("C" emacspeak-wizards-colors)
    ("d" deadgrep)
    ("c" emacspeak-wizards-color-wheel)
    ("e" emacspeak-we-xsl-map)
    ("h" emacspeak-wizards-how-many-matches)
    ("i" ibuffer)
    ("j" emacspeak-jabber-popup-roster)
    ("m" mspools-show)
    ("o" emacspeak-wizards-occur-header-lines)
    ("p" paradox-list-packages)
    ("q" emacspeak-wizards-quote)
    ("w" emacspeak-wizards-noaa-weather)
    ("r" jabber-activity-switch-to)
    ("SPC" emacspeak-jabber-speak-recent-message)
    ("s" emacspeak-emergency-tts-restart)
    ("t" emacspeak-speak-telephone-directory)
    ("u" emacspeak-wizards-units)
    ("v" emacspeak-wizards-vc-viewer)
    ("|" emacspeak-wizards-squeeze-blanks)
    ("y" ytel)
    ("" desktop-clear)) 
  "*Specifies personal key bindings for the audio desktop.
Bindings specified here are available on prefix key C-e x
for example, if you bind
`s' to command emacspeak-emergency-tts-restart
then that command will be available on key C-e x s.

KEYS should be a string constant in the format used for saving
keyboard macros (see `edmacro-mode').

Command is an interactive command or a prefix-command that can be
bound to a key. 

The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies an
interactive command that the key sequence executes."
  :group 'emacspeak
  :type '(repeat
          :tag "Emacspeak Personal Keymap"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set
  #'(lambda (sym val)
      (emacspeak-keymap-bindings-update emacspeak-personal-keymap val)
      (set-default
       sym
       (sort
        val
        #'(lambda (a b) (string-lessp (car a) (car b)))))))

(define-key  emacspeak-keymap "x" 'emacspeak-personal-keymap)

;;}}}
;;{{{ Create personal ctl-x map 

(defvar  emacspeak-personal-ctlx-keymap nil
  "Emacspeak personal-ctlx keymap")

(define-prefix-command 'emacspeak-personal-ctlx-keymap
  'emacspeak-personal-ctlx-keymap)

(defcustom emacspeak-personal-ctlx-keys nil
  "*Specifies personal-ctlx key bindings for use with C-e C-x for
the audio desktop. Bindings specified here are available on
prefix key C-e C-x for example, if you bind `C-s' to command
emacspeak-emergency-tts-restart then that command will be
available on key C-e C-x C-s. KEYS should be a string constant in
the format used for saving keyboard macros (see `edmacro-mode').

Command is an interactive command or a prefix-command that can be
bound to a key.

The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies an
interactive command that the key sequence executes."
  :group 'emacspeak
  :type '(repeat
          :tag "Emacspeak Personal-Ctlx Keymap"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set
  #'(lambda (sym val)
      (emacspeak-keymap-bindings-update emacspeak-personal-ctlx-keymap val)
      (set-default sym
                   (sort
                    val
                    #'(lambda (a b) (string-lessp (car a) (car b)))))))

(define-key  emacspeak-keymap "\C-x" 'emacspeak-personal-ctlx-keymap)

;;}}}
;;{{{ Create a super keymap that users can put personal commands

(defvar  emacspeak-super-keymap nil
  "Emacspeak super keymap")

(define-prefix-command 'emacspeak-super-keymap   'emacspeak-super-keymap)

(defcustom emacspeak-super-keys 
  '(
    ("'" emacspeak-vlc)
    ("SPC"  emacspeak-wizards-scratch)
    ("c" calculator)
    ("d" emacspeak-dired-downloads)
    ("." emacspeak-wizards-shell-directory-reset)
    ("a" emacspeak-wizards-execute-asynchronously)
    ("f" flyspell-mode)
    ("g" emacspeak-google-tts-region)
    ("j" ido-imenu-anywhere)
    ("o" org-switchb)
    ("n" emacspeak-wizards-google-news)
    ("C-n" emacspeak-wizards-google-headlines)
    ("q" emacspeak-wizards-iex-show-quote)
    ("r" soundscape-restart)
    ("s" soundscape)
    ("t" soundscape-toggle)
    ("u" soundscape-update-mood)
    ("S" soundscape-stop)
    ("e" elfeed)
    ("h" emacspeak-m-player-from-media-history)
    ("l" emacspeak-m-player-locate-media)
    ("m" emacspeak-wizards-view-buffers-filtered-by-this-mode)
    ("p" proced)
    ("R" emacspeak-webspace-feed-reader)
    ("w" define-word))
  "*Specifies super key bindings for the audio desktop. You can
turn the right `windows menu' keys on your Linux PC keyboard into
a `super' key on Linux by having it emit the sequence `C-x@s'.

Bindings specified here are available on prefix key `super' for
example, if you bind `s' to command
emacspeak-emergency-tts-restart then that command will be
available on key `super s'. KEYS should be a string constant in
the format used for saving keyboard macros (see `edmacro-mode').

Command is an interactive command or a prefix-command that can be
bound to a key.

The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies an
interactive command that the key sequence executes."
  :group 'emacspeak
  :type '(repeat
          :tag "Emacspeak Super Keymap"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set
  #'(lambda (sym val)
      (emacspeak-keymap-bindings-update emacspeak-super-keymap  val)
      (set-default sym
                   (sort
                    val
                    #'(lambda (a b) (string-lessp (car a) (car b)))))))

(global-set-key "\C-x@s" 'emacspeak-super-keymap)

;;}}}
;;{{{ Create a alt keymap that users can put personal commands

(defvar  emacspeak-alt-keymap nil "Emacspeak alt keymap")

(define-prefix-command 'emacspeak-alt-keymap   'emacspeak-alt-keymap)

(defcustom emacspeak-alt-keys 
  '(
    ("," emacspeak-eldoc-speak-doc)
    ("SPC" emacspeak-eww-smart-tabs)
    ("a" emacspeak-feeds-atom-display)
    ("b" sox-binaural)
    ("c" emacspeak-wizards-view-buffers-filtered-by-this-mode)
    ("d" sdcv-search-input)
    ("e" eww)
    ("f" emacspeak-wizards-find-file-as-root)
    ("g" rg-dwim)
    ("g" rg)
    ("i" emacspeak-wizards-iheart)
    ("l" eww-open-file)
    ("m" magit-status)
    ("n" emacspeak-wizards-cycle-to-next-buffer)
    ("o" emacspeak-feeds-opml-display)
    ("p" emacspeak-wizards-cycle-to-previous-buffer)
    ("q" emacspeak-wizards-iex-show-price)
    ("r" emacspeak-feeds-rss-display)
    ("s" emacspeak-wizards-tune-in-radio-search)
    ("t" emacspeak-wizards-tune-in-radio-browse)
    ("u" emacspeak-m-player-url)
    ("v" visual-line-mode)
    ("y" emacspeak-m-player-youtube-player)) 
  "*Specifies alt key bindings for the audio desktop. You can turn the
`Pause' key on your Linux PC keyboard into a `alt' key on Linux by
having it emit the sequence `C-x@a'.

Bindings specified here are available on prefix key `alt' (not to be
 confused with alt==meta) for example, if you bind `s' to command
 emacspeak-emergency-tts-restart then that command will be available
 on key `ALT s'

KEYS should be a string constant in the format used for saving
keyboard macros (see `edmacro-mode').

Command is an interactive command or a prefix-command that can be
bound to a key.

The value of this variable is an association list. The car of each
element specifies a key sequence. The cdr specifies an interactive
command that the key sequence executes."
  :group 'emacspeak
  :type '(repeat
          :tag "Emacspeak Alt Keymap"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set #'(lambda (sym val)
           (emacspeak-keymap-bindings-update emacspeak-alt-keymap val)
           (set-default sym
                        (sort
                         val
                         #'(lambda (a b) (string-lessp (car a) (car b)))))))

(global-set-key "\C-x@a"
                'emacspeak-alt-keymap)

;;}}}
;;{{{ Create a C-z keymap that is customizable 

;;; 2020: Suspending emacs with C-z is something I've not done in 30
;;; years.
;;; Turn it into a useful prefix key.

(defvar  emacspeak-ctl-z-keymap nil
  "Emacspeak ctl-z keymap")

(define-prefix-command 'emacspeak-ctl-z-keymap   'emacspeak-ctl-z-keymap)

(defcustom emacspeak-ctl-z-keys 
  '(
    ("z" suspend-frame)
    )
  "*Specifies ctl-z  key bindings for the audio desktop. 

Bindings specified here are available on prefix key `C-z' for
example, if you bind `zb' to command `suspend-frame ' then that command
will be available on key `C-z z'.

KEYS should be a string constant in the format used for saving
keyboard macros (see `edmacro-mode').

Command is an interactive command or a prefix-command that can be
bound to a key. 

The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies an
interactive command that the key sequence executes."
  :group 'emacspeak
  :type '(repeat
          :tag "Emacspeak C-Z  Keys"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set
  #'(lambda (sym val)
      (emacspeak-keymap-bindings-update emacspeak-ctl-z-keymap val)
      (set-default sym
                   (sort
                    val
                    #'(lambda (a b) (string-lessp (car a) (car b)))))))

(global-set-key (kbd "C-z") 'emacspeak-ctl-z-keymap)

;;}}}
;;{{{ Create a hyper keymap that users can put personal commands

(defvar  emacspeak-hyper-keymap nil
  "Emacspeak hyper keymap")

(define-prefix-command 'emacspeak-hyper-keymap   'emacspeak-hyper-keymap)

(defcustom emacspeak-hyper-keys 
  '(
    ("," switch-to-prev-buffer)
    ("." switch-to-next-buffer)
    ("C-e" eshell)
    ("TAB" hippie-expand)
    ("C-r" flx-isearch-backward)
    ("C-s" flx-isearch-forward)
    ("C-u" emacspeak-feeds-browse)
    (":" emacspeak-wizards-view-buffers-filtered-by-m-player-mode)
    (";" emacspeak-m-player-using-openal)
    ("'" emacspeak-m-player-using-hrtf)
    ("B" eww-list-bookmarks)
    ("a" emacspeak-wizards-term)
    ("b" eww-list-buffers)
    ("c" browse-url-chrome)
    ("d" magit-dispatch)
    ("e" gmaps)
    ("f" find-file)
    ("g" gnus)
    ("h" emacspeak-org-capture-link)
    ("i" yas-insert-snippet)
    ("j" emacspeak-wizards-shell-toggle)
    ("k" emacspeak-webspace-knowledge-search)
    ("l" emacspeak-librivox)
    ("m" vm)
    ("o" helm-mini)
    ("p" emacspeak-wizards-pdf-open)
    ("s" emacspeak-wizards-shell)
    ("t" twit)
    ("u" list-unicode-display)
    ("w" emacspeak-wizards-noaa-weather)
    ("y" yas-expand))
  "*Specifies hyper key bindings for the audio desktop. Emacs can
use the `hyper' key as a modifier key. You can turn the `windows'
keys on your Linux PC keyboard into a `hyper' key on Linux by
having it emit the sequence `C-x@h'.

Bindings specified here are available on prefix key `hyper' for
example, if you bind `b' to command `bbdb ' then that command
will be available on key `hyper b'.

KEYS should be a string constant in the format used for saving
keyboard macros (see `edmacro-mode').

Command is an interactive command or a prefix-command that can be
bound to a key. 

The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies an
interactive command that the key sequence executes."
  :group 'emacspeak
  :type '(repeat
          :tag "Emacspeak Hyper Keys"
          (list
           :tag "Key Binding"
           (key-sequence :tag "Key")
           (ems-interactive-command :tag "Command")))
  :set
  #'(lambda (sym val)
      (emacspeak-keymap-bindings-update emacspeak-hyper-keymap val)
      (set-default sym
                   (sort
                    val
                    #'(lambda (a b) (string-lessp (car a) (car b)))))))

(global-set-key "\C-x@h" 'emacspeak-hyper-keymap)

;;}}}
;;{{{ Keymaps <-> Org (text) Files :

;;; This makes it easy to consolidate personal bindings across machines.
;;; It also protects against custom losing settings due to Custom accidents.
;;;

(defun emacspeak-keymap-bindings-from-org (variable filename)
  "Load bindings from a specified file."
  (interactive "vVariable: \nfFilename: ")
  (let ((bindings nil))
    (with-temp-buffer
      "org-to-map"
      (insert-file filename)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((fields
               (split-string
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                " " 'omit-nulls)))
          (push
           (list (cl-first fields) (intern (cl-second fields)))
           bindings))
        (forward-line 1)))
    (setq bindings (nreverse (copy-sequence bindings)))
    (set variable  bindings)
    (customize-save-variable variable bindings)))

(defun emacspeak-keymap-bindings-to-org (variable filename)
  "Persists mapping to org file."
  (interactive "vVariable: \nfFilename: ")
  (let ((buffer (find-file-noselect  filename)))
    (with-current-buffer
        buffer
      (goto-char (point-max))
      (cl-loop
       for binding  in (symbol-value variable) do
       (insert (format "%s %s\n" (cl-first binding) (cl-second binding))))
      (save-buffer buffer))
    (switch-to-buffer buffer)))

;;}}}
;;{{{ Helper: recover end-of-line

(defun emacspeak-keymap-recover-eol ()
  "Recover end-of-line."
  (cl-declare (special emacspeak-prefix))
  (global-set-key (concat emacspeak-prefix "e") 'end-of-line)
  (global-set-key (concat emacspeak-prefix emacspeak-prefix) 'end-of-line))

;;}}}
;;{{{ Global Bindings From Other Modules:

(global-set-key (kbd "C-x r e") 'emacspeak-eww-open-mark)

;;}}}
;;{{{Mode-Specific Bindings:

(define-key emacs-lisp-mode-map (kbd "C-c SPC") 'emacspeak-wizards-insert-elisp-prefix)

;;}}}
(provide 'emacspeak-keymap)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
