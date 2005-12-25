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
          (let ((values
                 (condition-case nil
                     (sort val
                           #'(lambda (a b)
                               (string-lessp(car a) (car b))))
                   (error val))))
            (mapc
             (lambda (binding)
               (let ((key (car binding))
                     (command (cdr binding )))
                 (when (string-match "\\[.+]" key)
                   (setq key  (car (read-from-string key))))
                 (define-key emacspeak-personal-keymap  key command)))
             values)
            (set-default sym values))))

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
(let ((values
       (condition-case nil 
	   (sort val
		 #'(lambda (a b)
		     (string-lessp (car a) (car b ))))
	 (error val))))
  (mapc
   (lambda (binding)
     (let ((key (car binding))
           (command (cdr binding )))
       (when (string-match "\\[.+]" key)
         (setq key (car (read-from-string key))))
       (define-key emacspeak-super-keymap key command)))
   values)
  (set-default sym values))))

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
(let ((values
       (condition-case nil
           (sort val
		 #'(lambda (a b)
		     (string-lessp (car a) (car b))))
         (error val))))
  (mapc
   (lambda (binding)
     (let ((key (car binding))
           (command (cdr binding )))
       (when (string-match "\\[.+]" key)
         (setq key (car (read-from-string key))))
       (define-key emacspeak-alt-keymap key command)))
   values)
  (set-default sym values))))

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
(let ((values
       (condition-case nil
           (sort val
		 #'(lambda (a b)
		     (string-lessp (car a) (car b))))
         (error val))))
  (mapc
   (lambda (binding)
     (let ((key (car binding))
           (command (cdr binding )))
       (when (string-match "\\[.+]" key)
         (setq key (car (read-from-string key))))
       (define-key emacspeak-hyper-keymap key command)))
   values)
  (set-default sym values))))

(global-set-key "\C-x@h"
		'emacspeak-hyper-keymap)

;;}}}
;;{{{ helper: emacspeak-keymap-update 

(defsubst emacspeak-keymap-update (keymap binding)
  "Update keymap with specified binding."
  (define-key keymap (first binding) (second binding)))

;;}}}
;;{{{  The Emacspeak key  bindings.

(define-key help-map "E"
  'emacspeak-websearch-emacspeak-archive)
(define-key help-map "V" 'customize-variable)
(define-key help-map " " 'customize-group)
(define-key help-map "B" 'customize-browse)
(define-key help-map "G" 'customize-group)
(define-key help-map "M" 'emacspeak-speak-popup-messages)
(define-key help-map emacspeak-prefix
  'emacspeak-describe-emacspeak)
;;;Let's bind find-func on useful help keys 
(define-key help-map "\M-f" 'find-function)
(define-key help-map "\M-F" 'find-function-at-point)
(define-key help-map "\M-k" 'find-function-on-key)
(define-key help-map "\M-v" 'find-variable)
(define-key help-map "\M-V" 'find-variable-at-point)

(define-key emacspeak-keymap '[aleft]
  'emacspeak-speak-this-buffer-previous-display)
(define-key emacspeak-keymap '[right]
  'emacspeak-speak-this-buffer-next-display)
(define-key emacspeak-keymap '[(control left)]
  'emacspeak-select-this-buffer-previous-display)
(define-key emacspeak-keymap '[(control right)]
  'emacspeak-select-this-buffer-next-display)
(define-key emacspeak-keymap "/" 'emacspeak-speak-this-buffer-other-window-display)
(define-key emacspeak-keymap '[down]
  'emacspeak-read-next-line)
(define-key emacspeak-keymap '[(control down)] 'emacspeak-cvs-get-anonymous)
(define-key emacspeak-keymap '[up]  'emacspeak-read-previous-line)
(define-key emacspeak-keymap "v" 'emacspeak-view-register)
(define-key emacspeak-keymap "w" 'emacspeak-speak-word)
(define-key emacspeak-keymap "W"
  'emacspeak-tapestry-select-window-by-name)
(define-key emacspeak-keymap "u" 'emacspeak-url-template-fetch)
(define-key emacspeak-keymap "\C-u" 'emacspeak-rss-browse)
(define-key emacspeak-keymap "U" 'emacspeak-websearch-usenet)
(define-key emacspeak-keymap "V" 'emacspeak-speak-version)
(define-key emacspeak-keymap "\C-v" 'view-mode)
(define-key emacspeak-keymap "t" 'emacspeak-speak-time )
(define-key emacspeak-keymap "T" 'emacspeak-view-emacspeak-tips )
(define-key emacspeak-keymap "s" 'dtk-stop)
(define-key emacspeak-keymap "r" 'emacspeak-speak-region)
(define-key emacspeak-keymap "R" 'emacspeak-speak-rectangle)
(define-key emacspeak-keymap "q" 'emacspeak-toggle-speak-messages)
(define-key emacspeak-keymap "n" 'emacspeak-speak-rest-of-buffer)
(define-key emacspeak-keymap "m" 'emacspeak-speak-mode-line)
(define-key emacspeak-keymap "\C-l"
  'emacspeak-speak-line-number)
(define-key emacspeak-keymap "|"
  'emacspeak-speak-line-set-column-filter)
(define-key emacspeak-keymap "^" 'emacspeak-filtertext)
(define-key emacspeak-keymap "\\" 'emacspeak-toggle-speak-line-invert-filter)
(define-key emacspeak-keymap "l" 'emacspeak-speak-line)
(define-key emacspeak-keymap "k" 'emacspeak-speak-current-kill )
(define-key emacspeak-keymap "\C-@" 'emacspeak-speak-current-mark )
(define-key emacspeak-keymap "\M-\C-@" 'emacspeak-speak-spaces-at-point)
(define-key emacspeak-keymap "\M-\C-k" 'kill-emacs)
(define-key emacspeak-keymap "i" 'emacspeak-tabulate-region)
(define-key emacspeak-keymap "I"  'emacspeak-speak-show-active-network-interfaces)
(define-key emacspeak-keymap "\C-t" 'emacspeak-table-find-file)
(define-key emacspeak-keymap "h" 'emacspeak-speak-help)
(define-key emacspeak-keymap "j" 'emacspeak-hide-or-expose-block)
(define-key emacspeak-keymap "\C-j" 'emacspeak-hide-speak-block-sans-prefix)
(define-key emacspeak-keymap  "f"
  'emacspeak-speak-buffer-filename )
(define-key emacspeak-keymap  "\M-f"
  'emacspeak-frame-label-or-switch-to-labelled-frame )
(define-key emacspeak-keymap  "\C-f" 'emacspeak-freeamp-prefix-command )
(define-key emacspeak-keymap "c" 'emacspeak-speak-char)
(define-key emacspeak-keymap "b" 'emacspeak-speak-buffer)
(define-key emacspeak-keymap "a" 'emacspeak-speak-message-again )
(define-key emacspeak-keymap "\C-s" 'tts-restart )
(define-key emacspeak-keymap "\C-q"
  'emacspeak-toggle-comint-autospeak)
(define-key emacspeak-keymap "o" 'emacspeak-toggle-comint-output-monitor)
(define-key emacspeak-keymap "\C-m"  'emacspeak-speak-continuously)
(define-key emacspeak-keymap "\C-i" 'emacspeak-table-display-table-in-region)
(define-key emacspeak-keymap "\C-h" 'emacspeak-learn-mode)
(define-key emacspeak-keymap "H" 'emacspeak-speak-browse-linux-howto)
(define-key emacspeak-keymap "\M-\C-b" 'emacspeak-submit-bug )
(define-key emacspeak-keymap "\C-b" 'emacspeak-daisy-open-book)
(define-key emacspeak-keymap "\"" 'emacspeak-speak-sexp-interactively)
(define-key emacspeak-keymap "p" 'dtk-pause)
(define-key emacspeak-keymap "]" 'emacspeak-speak-page-interactively)
(define-key emacspeak-keymap "{" 'emacspeak-speak-paragraph)
(define-key emacspeak-keymap "[" 'emacspeak-speak-page)
(define-key emacspeak-keymap "P" 'emacspeak-speak-paragraph-interactively)
(define-key emacspeak-keymap "M" 'emacspeak-speak-minor-mode-line)
(define-key emacspeak-keymap "L" 'emacspeak-speak-line-interactively)
(define-key emacspeak-keymap "\M-h" 'emacspeak-speak-hostname)
(define-key emacspeak-keymap "B" 'emacspeak-speak-buffer-interactively)
(define-key emacspeak-keymap "A" 'emacspeak-appt-repeat-announcement)
(define-key emacspeak-keymap "?"
  'emacspeak-websearch-dispatch )
(define-key emacspeak-keymap "," 'emacspeak-speak-browse-buffer )
(define-key emacspeak-keymap ":" 'emacspeak-realaudio )
(define-key emacspeak-keymap ";" 'emacspeak-multimedia)
(define-key emacspeak-keymap "C" 'emacspeak-customize)
(define-key emacspeak-keymap "\C-o" 'emacspeak-ocr )
(define-key emacspeak-keymap "\C-c"
  'emacspeak-clipboard-copy)
(define-key emacspeak-keymap "\M-c"
  'emacspeak-copy-current-file)
(define-key emacspeak-keymap "\M-l"
  'emacspeak-link-current-file)
(define-key emacspeak-keymap "\M-s"
  'emacspeak-symlink-current-file)
(define-key emacspeak-keymap "!"
  'emacspeak-speak-run-shell-command)
(define-key emacspeak-keymap "@" 'emacspeak-speak-message-at-time)
(define-key emacspeak-keymap "#" 'emacspeak-gridtext)
(define-key emacspeak-keymap "\C-y" 'emacspeak-clipboard-paste)
(define-key emacspeak-keymap "\C-p" 'emacspeak-speak-previous-window)
(define-key emacspeak-keymap "\C-n" 'emacspeak-speak-next-window )
(define-key emacspeak-keymap "(" 'emacspeak-aumix)
(define-key emacspeak-keymap ")" 'emacspeak-sounds-select-theme)
(define-key emacspeak-keymap ")" 'emacspeak-sounds-select-theme)
(define-key emacspeak-keymap "\d" 'cd-tool)
(define-key emacspeak-keymap "'" 'emacspeak-speak-sexp)
(define-key emacspeak-keymap "=" 'emacspeak-speak-current-column)
(define-key emacspeak-keymap "%" 'emacspeak-speak-current-percentage)
(define-key emacspeak-keymap "<" 'emacspeak-speak-previous-field)
(define-key emacspeak-keymap "." 'emacspeak-speak-current-field)
(define-key emacspeak-keymap ">"  'emacspeak-speak-next-field)
(define-key emacspeak-keymap " " 'dtk-resume)
(define-key emacspeak-keymap "\C-w"
  'emacspeak-speak-window-information)
(define-key emacspeak-keymap "\M-w" 'emacspeak-toggle-which-function)
(define-key emacspeak-keymap   "\C-a"
  'emacspeak-toggle-auditory-icons )
(define-key emacspeak-keymap "\C-r" 'emacspeak-root)
(define-key emacspeak-keymap "\M-\C-r" 'emacspeak-eterm-remote-term)
(define-key emacspeak-keymap "\M-r"
  'emacspeak-remote-connect-to-server)
(define-key emacspeak-keymap "\M-d"
  'emacspeak-pronounce-dispatch)
(define-key emacspeak-keymap "\M-b" 'emacspeak-speak-other-buffer)
(define-key emacspeak-keymap "\M-a" 'emacspeak-set-auditory-icon-player)
(define-key emacspeak-keymap "\M-m" 'emacspeak-toggle-mail-alert)
(define-key emacspeak-keymap "\M-v"
  'emacspeak-show-personality-at-point)
(define-key emacspeak-keymap "\M-p" 'emacspeak-show-property-at-point)
(define-key emacspeak-keymap "\M-t" 'emacspeak-tapestry-describe-tapestry)
(define-key emacspeak-keymap "\C-d" 'emacspeak-toggle-show-point)
;;; speaking specific windows:

(dotimes (i 10)
  (define-key emacspeak-keymap
    (format "%s" i )
    'emacspeak-speak-predefined-window ))
(define-key emacspeak-keymap "D"
  'emacspeak-view-emacspeak-doc)
(define-key emacspeak-keymap "N"
  'emacspeak-view-emacspeak-news)
(define-key emacspeak-keymap "F"
  'emacspeak-view-emacspeak-faq)
(define-key emacspeak-keymap "D"
  'emacspeak-view-emacspeak-doc)
(define-key emacspeak-keymap '[f1]
  'customize-saved)
(define-key emacspeak-keymap '[f11] 'emacspeak-wizards-shell-toggle)
;;; submap for setting dtk:
(define-key emacspeak-dtk-submap "z" 'emacspeak-zap-tts)
(define-key emacspeak-dtk-submap "t" 'emacspeak-dial-dtk)
(define-key emacspeak-dtk-submap "w" 'emacspeak-toggle-word-echo)
(define-key emacspeak-dtk-submap "V" 'tts-speak-version)
(define-key emacspeak-dtk-submap "v" 'voice-lock-mode)
(define-key emacspeak-dtk-submap "n" 'dtk-toggle-speak-nonprinting-chars)
(define-key emacspeak-dtk-submap "s" 'dtk-toggle-split-caps)
(define-key emacspeak-dtk-submap "o" 'dtk-toggle-strip-octals)
(define-key emacspeak-dtk-submap "r" 'dtk-set-rate)
(define-key emacspeak-dtk-submap "\C-m" 'dtk-set-chunk-separator-syntax)
(define-key emacspeak-dtk-submap " " 'dtk-toggle-splitting-on-white-space)
(define-key emacspeak-dtk-submap "R" 'dtk-reset-state)
(define-key emacspeak-dtk-submap "q" 'dtk-toggle-quiet )
(define-key emacspeak-dtk-submap "p" 'dtk-set-punctuations)
(define-key emacspeak-dtk-submap "l" 'emacspeak-toggle-line-echo)
(define-key emacspeak-dtk-submap "k" 'emacspeak-toggle-character-echo)
(define-key emacspeak-dtk-submap "i"
  'emacspeak-toggle-audio-indentation )
(define-key emacspeak-dtk-submap "I" 'dtk-toggle-stop-immediately-while-typing )
(define-key emacspeak-dtk-submap "f" 'dtk-set-character-scale)
(define-key emacspeak-dtk-submap "d" 'dtk-select-server)
(define-key emacspeak-dtk-submap "c" 'dtk-toggle-capitalization)
(define-key emacspeak-dtk-submap "C" 'dtk-toggle-allcaps-beep)
(define-key emacspeak-dtk-submap "b" 'dtk-toggle-debug)
(define-key emacspeak-dtk-submap "\M-\C-b" 'tts-show-debug-buffer)
(define-key emacspeak-dtk-submap "a" 'dtk-add-cleanup-pattern)

(dotimes (i 10)
  (define-key emacspeak-dtk-submap
    (format "%s" i )
    'dtk-set-predefined-speech-rate ))
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
(define-key help-map "\C-m" 'man)
(define-key help-map "\C-i" 'emacspeak-info-wizard)
(global-set-key '[left] 'emacspeak-backward-char)
(global-set-key '[right] 'emacspeak-forward-char)
(define-key emacspeak-keymap '[insert] 'emacspeak-emergency-tts-)

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
