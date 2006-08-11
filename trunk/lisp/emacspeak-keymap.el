;;; emacspeak-keymap.el --- Setup all keymaps and keybindings provided by Emacspeak
;;; $Id$
;;; $Author$ 
;;; Description:  Module for setting up emacspeak keybindings
;;; Keywords: Emacspeak
;;{{{  LCD Archive entry: 

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu 
;;; A speech interface to Emacs |
;;; $Date$ |
;;;  $Revision$ | 
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2004, T. V. Raman 
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

;;;Commentary:

;;; This module defines the emacspeak keybindings. 

;;; Code:

;;}}}
;;{{{ requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{  variables: 

(defvar emacspeak-prefix "\C-e"
  "Default prefix key used for emacspeak. ")
;;;###autoload 
(defvar emacspeak-keymap nil
  "Primary keymap used by emacspeak. ")

(defvar emacspeak-dtk-submap nil
  "Submap used for DTK commands. ")

;;}}}
;;{{{   Binding keymap and submap

(define-prefix-command 'emacspeak-prefix-command 'emacspeak-keymap )

(define-prefix-command  'emacspeak-dtk-submap-command
  'emacspeak-dtk-submap )
(global-set-key emacspeak-prefix 'emacspeak-prefix-command)

(define-key emacspeak-keymap "d"  'emacspeak-dtk-submap-command)

;;; fix what we just broke:-
(define-key emacspeak-keymap "e" 'end-of-line)
(define-key emacspeak-keymap "\C-e" 'end-of-line)

;;}}}
;;{{{ Create a keymap that users can put personal commands

;;; Adding keys using custom:
(defvar  emacspeak-personal-keymap nil
  "Emacspeak personal keymap")

(define-prefix-command 'emacspeak-personal-keymap   'emacspeak-personal-keymap)
;;;###autoload
(defcustom emacspeak-personal-keys nil
  "*Specifies personal key bindings for the audio desktop.
Bindings specified here are available on prefix key C-e x
for example, if you bind 
`s' to command emacspeak-emergency-tts-restart 
then that command will be available on key C-e x s

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. "
  :group 'emacspeak
  :type '(repeat :tag "Emacspeak Personal Keymap"
                 (cons  :tag "Key Binding"
                        (string :tag "Key")
                        (symbol :tag "Command")))
  :set '(lambda (sym val)
          (mapc
           (lambda (binding)
             (let ((key (car binding))
                   (command (cdr binding )))
               (when (string-match "\\[.+]" key)
                 (setq key  (car (read-from-string key))))
               (define-key emacspeak-personal-keymap  key command)))
           val)
          (set-default sym val)))

(define-key  emacspeak-keymap "x"
  'emacspeak-personal-keymap)

;;}}}
;;{{{ Create a super keymap that users can put personal commands

;;; I use the right windows menu key for super
;;on
;;; Adding keys using custom:
(defvar  emacspeak-super-keymap nil
  "Emacspeak super keymap")

(define-prefix-command 'emacspeak-super-keymap   'emacspeak-super-keymap)
;;;###autoload
(defcustom emacspeak-super-keys nil
  "*Specifies super key bindings for the audio desktop.
You can turn the right `windows menu' keys on your Linux PC keyboard into a `super' key
on Linux by having it emit the sequence `C-x@s'.

Bindings specified here are available on prefix key `super'
for example, if you bind 
`s' to command emacspeak-emergency-tts-restart 
then that command will be available on key `super  s'

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. "
  :group 'emacspeak
  :type '(repeat :tag "Emacspeak Super Keymap"
                 (cons  :tag "Key Binding"
                        (string :tag "Key")
                        (symbol :tag "Command")))
  :set '(lambda (sym val)
          (mapc
           (lambda (binding)
             (let ((key (car binding))
                   (command (cdr binding )))
               (when (string-match "\\[.+]" key)
                 (setq key (car (read-from-string key))))
               (define-key emacspeak-super-keymap key command)))
           val)
          (set-default sym val)))

(global-set-key "\C-x@s"
                'emacspeak-super-keymap)

;;}}}
;;{{{ Create a alt keymap that users can put personal commands

;;; I use the "pause" key to produce C-x@a -- which gives alt-
;;on
;;; Adding keys using custom:
(defvar  emacspeak-alt-keymap nil
  "Emacspeak alt keymap")

(define-prefix-command 'emacspeak-alt-keymap   'emacspeak-alt-keymap)
;;;###autoload
(defcustom emacspeak-alt-keys nil
  "*Specifies alt key bindings for the audio desktop.
You can turn the `Pause' key  on your Linux PC keyboard into a `alt' key
on Linux by having it emit the sequence `C-x@a'.

Bindings specified here are available on prefix key `alt'
(not to be confused with alt==meta)
for example, if you bind 
`s' to command emacspeak-emergency-tts-restart 
then that command will be available on key `ALT  s'

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. "
  :group 'emacspeak
  :type '(repeat :tag "Emacspeak Alt Keymap"
                 (cons  :tag "Key Binding"
                        (string :tag "Key")
                        (symbol :tag "Command")))
  :set '(lambda (sym val)
  (mapc
   (lambda (binding)
     (let ((key (car binding))
           (command (cdr binding )))
       (when (string-match "\\[.+]" key)
         (setq key (car (read-from-string key))))
       (define-key emacspeak-alt-keymap key command)))
   val)
  (set-default sym val)))

(global-set-key "\C-x@a"
                'emacspeak-alt-keymap)

;;}}}
;;{{{ Create a hyper keymap that users can put personal commands

;;; I use the windows key for hyper
;;on
;;; Adding keys using custom:
(defvar  emacspeak-hyper-keymap nil
  "Emacspeak hyper keymap")

(define-prefix-command 'emacspeak-hyper-keymap   'emacspeak-hyper-keymap)
;;;###autoload
(defcustom emacspeak-hyper-keys nil
  "*Specifies hyper key bindings for the audio desktop.
Emacs can use the `hyper' key as a modifier key.
You can turn the `windows' keys on your Linux PC keyboard into a `hyper' key
on Linux by having it emit the sequence `C-x@h'.

Bindings specified here are available on prefix key  `hyper'
for example, if you bind 
`b' to command `bbdb '
then that command will be available on key `hyper b'.

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. "
  :group 'emacspeak
  :type '(repeat :tag "Emacspeak Hyper Keys"
                 (cons  :tag "Key Binding"
                        (string :tag "Key")
                        (symbol :tag "Command")))
  :set '(lambda (sym val)
  (mapc
   (lambda (binding)
     (let ((key (car binding))
           (command (cdr binding )))
       (when (string-match "\\[.+]" key)
         (setq key (car (read-from-string key))))
       (define-key emacspeak-hyper-keymap key command)))
   val)
  (set-default sym val)))

(global-set-key "\C-x@h"
                'emacspeak-hyper-keymap)

;;}}}
;;{{{ helper: emacspeak-keymap-update 

(defsubst emacspeak-keymap-update (keymap binding)
  "Update keymap with specified binding."
  (define-key keymap (first binding) (second binding)))

;;}}}
;;{{{  The Emacspeak key  bindings.

;;; help map additions:

(loop for binding in 
      '(
        ( " " customize-group)
        ( "B" customize-browse)
        ( "E" emacspeak-websearch-emacspeak-archive)
        ( "G" customize-group)
        ( "M" emacspeak-speak-popup-messages)
        ( "V" customize-variable)
        ( "\M-F" find-function-at-point)
        ( "\M-V" find-variable-at-point)
        ( "\M-f" find-function)
        ( "\M-k" find-function-on-key)
        ( "\M-v" find-variable)
        ("\C-e"   emacspeak-describe-emacspeak)
        ("\C-i" emacspeak-info-wizard)
        ("\C-l" emacspeak-learn-emacs-mode)
        ("\C-m" man)
        ("\C-s" customize-saved)
        )
      do
      (emacspeak-keymap-update help-map binding))

;;; emacspeak-keymap bindings:
(loop for binding in
      '(
        (  "\C-a" emacspeak-toggle-auditory-icons )
        ( "\C-f" emacspeak-freeamp-prefix-command )
        ( "\M-f" emacspeak-frame-label-or-switch-to-labelled-frame )
        (" " dtk-resume)
        ("!" emacspeak-speak-run-shell-command)
        ("'" emacspeak-speak-sexp)
        ("#" emacspeak-gridtext)
        ("%" emacspeak-speak-current-percentage)
        ("(" emacspeak-aumix)
        (")" emacspeak-sounds-select-theme)
        ("," emacspeak-speak-browse-buffer )
        ("." emacspeak-speak-current-field)
        ("/" emacspeak-speak-this-buffer-other-window-display)
        (":" emacspeak-realaudio )
        (";" emacspeak-multimedia)
        ("<" emacspeak-speak-previous-field)
        ("=" emacspeak-speak-current-column)
        (">"  emacspeak-speak-next-field)
        ("?" emacspeak-websearch-dispatch )
        ("@" emacspeak-speak-message-at-time)
        ("A" emacspeak-appt-repeat-announcement)
        ("B" emacspeak-speak-buffer-interactively)
        ("C" emacspeak-customize)
        ("F" emacspeak-view-emacspeak-faq)
        ("H" emacspeak-speak-browse-linux-howto)
        ("I"  emacspeak-speak-show-active-network-interfaces)
        ("L" emacspeak-speak-line-interactively)
        ("M" emacspeak-speak-minor-mode-line)
        ("N" emacspeak-view-emacspeak-news)
        ("P" emacspeak-speak-paragraph-interactively)
        ("R" emacspeak-speak-rectangle)
        ("T" emacspeak-view-emacspeak-tips )
        ("U" emacspeak-websearch-usenet)
        ("V" emacspeak-speak-version)
        ("W" emacspeak-tapestry-select-window-by-name)
        ("[" emacspeak-speak-page)
        ("\"" emacspeak-speak-sexp-interactively)
        ("\C-@" emacspeak-speak-current-mark )
        ("\C-b" emacspeak-daisy-open-book)
        ("\C-c" emacspeak-clipboard-copy)
        ("\C-d" emacspeak-toggle-show-point)
        ("\C-i" emacspeak-table-display-table-in-region)
        ("\C-j" emacspeak-hide-speak-block-sans-prefix)
        ("\C-l" emacspeak-speak-line-number)
        ("\C-m"  emacspeak-speak-continuously)
        ("\C-n" emacspeak-speak-next-window )
        ("\C-o" emacspeak-ocr )
        ("\C-p" emacspeak-speak-previous-window)
        ("\C-q" emacspeak-toggle-comint-autospeak)
        ("\C-r" emacspeak-root)
        ("\C-s" tts-restart )
        ("\C-t" emacspeak-table-find-file)
        ("\C-u" emacspeak-rss-browse)
        ("\C-v" view-mode)
        ("\C-w" emacspeak-speak-window-information)
        ("\C-y" emacspeak-clipboard-paste)
        ("\M-\C-@" emacspeak-speak-spaces-at-point)
        ("\M-\C-b" emacspeak-submit-bug )
        ("\M-\C-k" kill-emacs)
        ("\M-\C-r" emacspeak-eterm-remote-term)
        ("\M-a" emacspeak-set-auditory-icon-player)
        ("\M-b" emacspeak-speak-other-buffer)
        ("\M-c" emacspeak-copy-current-file)
        ("\M-d" emacspeak-pronounce-dispatch)
        ("\M-h" emacspeak-speak-hostname)
        ("\M-l" emacspeak-link-current-file)
        ("\M-m" emacspeak-toggle-mail-alert)
        ("\M-r" emacspeak-remote-connect-to-server)
        ("\M-s" emacspeak-symlink-current-file)
        ("\M-t" emacspeak-tapestry-describe-tapestry)
        ("\M-v" emacspeak-show-personality-at-point)
        ("\M-w" emacspeak-toggle-which-function)
        ("\\" emacspeak-toggle-speak-line-invert-filter)
        ("\d" cd-tool)
        ("]" emacspeak-speak-page-interactively)
        ("^" emacspeak-filtertext)
        ("a" emacspeak-speak-message-again )
        ("b" emacspeak-speak-buffer)
        ("c" emacspeak-speak-char)
        ("f" emacspeak-speak-buffer-filename )
        ("h" emacspeak-speak-help)
        ("i" emacspeak-tabulate-region)
        ("j" emacspeak-hide-or-expose-block)
        ("k" emacspeak-speak-current-kill )
        ("l" emacspeak-speak-line)
        ("m" emacspeak-speak-mode-line)
        ("n" emacspeak-speak-rest-of-buffer)
        ("o" emacspeak-toggle-comint-output-monitor)
        ("p" dtk-pause)
        ("q" emacspeak-toggle-speak-messages)
        ("r" emacspeak-speak-region)
        ("s" dtk-stop)
        ("t" emacspeak-speak-time )
        ("u" emacspeak-url-template-fetch)
        ("v" emacspeak-view-register)
        ("w" emacspeak-speak-word)
        ("{" emacspeak-speak-paragraph)
        ("|" emacspeak-speak-line-set-column-filter)
        ([(control down)] emacspeak-cvs-get-anonymous)
        ([(control left)] emacspeak-select-this-buffer-previous-display)
        ([(control right)] emacspeak-select-this-buffer-next-display)
        ([aleft] emacspeak-speak-this-buffer-previous-display)
        ([down] emacspeak-read-next-line)
        ([f1] emacspeak-learn-emacs-mode)
        ([f11] emacspeak-wizards-shell-toggle)
        ([insert] emacspeak-emergency-tts-restart)
        ([delete] emacspeak-ssh-tts-restart)
        ([right] emacspeak-speak-this-buffer-next-display)
        ([up]  emacspeak-read-previous-line)
        )
      do
      (emacspeak-keymap-update emacspeak-keymap binding))

(dotimes (i 10)
  (define-key emacspeak-keymap   (format "%s" i )
    'emacspeak-speak-predefined-window ))

(loop for binding in
      '(
        (" " dtk-toggle-splitting-on-white-space)
        ("C" dtk-toggle-allcaps-beep)
        ("I" dtk-toggle-stop-immediately-while-typing )
        ("R" dtk-reset-state)
        ("V" tts-speak-version)
        ("\C-m" dtk-set-chunk-separator-syntax)
        ("\M-\C-b" tts-show-debug-buffer)
        ("a" dtk-add-cleanup-pattern)
        ("b" dtk-toggle-debug)
        ("c" dtk-toggle-capitalization)
        ("d" dtk-select-server)
        ("f" dtk-set-character-scale)
        ("i" emacspeak-toggle-audio-indentation )
        ("k" emacspeak-toggle-character-echo)
        ("l" emacspeak-toggle-line-echo)
        ("n" dtk-toggle-speak-nonprinting-chars)
        ("o" dtk-toggle-strip-octals)
        ("p" dtk-set-punctuations)
        ("q" dtk-toggle-quiet )
        ("r" dtk-set-rate)
        ("s" dtk-toggle-split-caps)
        ("t" emacspeak-dial-dtk)
        ("v" voice-lock-mode)
        ("w" emacspeak-toggle-word-echo)
        ("z" emacspeak-zap-tts)
        )
      do
      (emacspeak-keymap-update emacspeak-dtk-submap binding))

(dotimes (i 10) (define-key emacspeak-dtk-submap   (format "%s" i )   'dtk-set-predefined-speech-rate ))

;;; Put these in the global map:
(global-set-key '[(control left)] 'emacspeak-previous-frame-or-buffer)
(global-set-key '[(control right)] 'emacspeak-next-frame-or-buffer)
(global-set-key '[pause] 'dtk-stop)
(global-set-key '[(control down)] 'emacspeak-mark-forward-mark)
(global-set-key '[(control up)] 'emacspeak-mark-backward-mark)
(global-set-key '[shift up] 'emacspeak-skip-blank-lines-backward)
(global-set-key '[shift down] 'emacspeak-skip-blank-lines-forward)
(global-set-key '[27 up]  'emacspeak-owindow-previous-line)
(global-set-key  '[27 down]  'emacspeak-owindow-next-line)
(global-set-key  '[27 prior]  'emacspeak-owindow-scroll-down)
(global-set-key  '[27 next]  'emacspeak-owindow-scroll-up)
(global-set-key  '[27 select]  'emacspeak-owindow-speak-line)
(global-set-key '[left] 'emacspeak-backward-char)
(global-set-key '[right] 'emacspeak-forward-char)

;;}}}
;;{{{ Hacking minibuffer maps:

(declaim (special  minibuffer-local-must-match-map
                   minibuffer-local-map
                   minibuffer-local-completion-map
                   minibuffer-local-ns-map))
(or (string-match  "Xemacs" emacs-version)
    (mapcar
     (function (lambda (map)
                 (and map 
                      (define-key map 
                        "\C-o"
                        'emacspeak-switch-to-completions-window))))
     (list minibuffer-local-must-match-map
           minibuffer-local-map
           minibuffer-local-completion-map
           minibuffer-local-ns-map)))

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
relief."
  (interactive "kPress the key you would like to use as the emacspeak prefix")
  (declare (special emacspeak-prefix))
  (let ((current-use (lookup-key  global-map prefix-key)))
    (global-set-key prefix-key 'emacspeak-prefix-command)
    (unless (eq  current-use 'emacspeak-prefix-command)
      (global-set-key (concat prefix-key prefix-key) current-use)
      (message "Use %s %s to execute %s since %s is now the emacspeak prefix"
               prefix-key prefix-key current-use
               prefix-key))))

;;}}}
;;{{{  removing emacspeak-self-insert-command in non-edit modes.
;;;###autoload 
(defun emacspeak-keymap-remove-emacspeak-edit-commands
  (keymap)
  "We define keys that invoke editting commands to be undefined"
  (loop for k in
        (where-is-internal 'emacspeak-self-insert-command
                           keymap)
        do
        (define-key keymap k 'undefined )))
;;}}}
(provide 'emacspeak-keymap)

;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
