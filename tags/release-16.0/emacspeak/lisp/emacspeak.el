;;; emacspeak.el --- Emacspeak -- The Complete Audio Desktop
;;; $Id$
;;; $Author$
;;; Description:  Emacspeak: A speech interface to Emacs
;;; Keywords: Emacspeak, Speech, Dectalk,
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
;;;Copyright (C) 1995 -- 2002, T. V. Raman 
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
;;{{{ required modules


;;; Commentary:
;;;The complete audio desktop.
;;; Code:

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'backquote)

(require 'dtk-speak)
(require 'voice-lock)
(require 'emacspeak-sounds)
(require 'emacspeak-speak)
(require 'emacspeak-keymap)
(require 'emacspeak-fix-interactive)
(require 'custom)
;;}}}
;;{{{  Introduction:

;;;Commentary:

;;;Emacspeak extends Emacs to be a fully functional audio desktop.
;;; This is the main emacspeak module.
;;; It actually does very little:
;;; It loads the various parts of the system.

;;}}}
;;{{{  Customize groups 

(defgroup emacspeak nil
  "Emacspeak: The Complete Audio Desktop  "
  :link '(url-link :tag "SourceForge" "http://emacspeak.sf.net")
  :link '(url-link :tag "Cornell" "http://www.cs.cornell.edu/home/raman/emacspeak")
  :link '(url-link :tag "Papers" "http://emacspeak.sf.net/publications")
  :link '(url-link :tag "Mail"
                   "http://www.cs.vassar.edu/~priestdo/emacspeak/")
  :link '(url-link :tag "Search" "http://www.cs.vassar.edu/cgi-bin/emacspeak-search")
  :link '(url-link :tag "Applications"
                   "http://emacspeak.sf.net/applications.html")
  :link '(url-link :tag "Guide"
                   "http://emacspeak.sf.net/user-guide")
  :link '(url-link :tag "Tips"
                   "http://emacspeak.sf.net/tips.html")
  :link   (list 'file-link :tag "NEWS" (expand-file-name "etc/NEWS" emacspeak-directory))
  :link   (list 'file-link :tag "FAQ" (expand-file-name "etc/FAQ" emacspeak-directory))
  :link '(custom-manual "(emacspeak)Top")
;;; end links 
  :prefix "emacspeak-"
  :group 'applications
  :group 'accessibility
  )



;;}}}
;;{{{  Setting up things:

(defconst emacspeak-version
  (let ((x "$Revision$"))
    (string-match "[0-9.]+" x)
    (substring x (match-beginning 0)
               (match-end 0)))
  "Version number for Emacspeak.")

(defcustom emacspeak-startup-hook nil
  "Hook to run after starting emacspeak."
  :type 'sexp
  :group 'emacspeak)

;;}}}
;;{{{ Emacspeak:

;;{{{  Make these interactive commands speak:

(defvar emacspeak-emacs-commands-to-fix
  '(
    add-completions-from-file
    ange-ftp-copy-file
    ange-ftp-delete-file
    ange-ftp-kill-ftp-process
    ange-ftp-nslookup-host
    ange-ftp-rename-file
    ange-ftp-set-user
    append-to-file
    append-to-register
    byte-force-recompile
    byte-recompile-directory
    cd
    comint-run
    command-apropos
    copy-rectangle-to-register
    copy-to-buffer
    copy-to-register
    customize-apropos
    customize-apropos-faces
    customize-apropos-groups
    customize-apropos-options
    debug-on-entry
    debugger-return-value
    define-mail-abbrev
    define-mail-alias
    describe-key
    describe-key-briefly
    desktop-save
    dmacro-load
    dtk-set-character-scale
    dtk-set-rate
    emacspeak-dial-dtk
    emacspeak-frame-label-or-switch-to-labelled-frame
    emacspeak-generate-documentation
    emacspeak-keymap-choose-new-emacspeak-prefix
    find-file
    find-file-literally
    find-file-other-frame
    find-file-other-window
    find-file-read-only
    find-file-read-only-other-frame
    find-file-read-only-other-window
    fold-goto-line
    global-unset-key
    goto-line
    help-make-xrefs
    ielm-change-working-buffer
    increment-register
    insert-file
    insert-file-literally
    insert-register
    insert-variable
    jump-to-register
    list-text-properties-at
    load-file
    load-library
    local-unset-key
    locate-library
    mail-attach-file
    mail-fcc
    make-frame-on-display
    make-obsolete
    makefile-insert-macro
    makefile-insert-target
    message-resend
    mime-include-audio
    mime-include-external-anonftp
    mime-include-external-ftp
    mime-include-gif
    mime-include-jpeg
    mime-include-postscript
    mime-include-raw-binary
    mime-include-raw-nonbinary
    nonincremental-re-search-backward
    nonincremental-re-search-forward
    nonincremental-search-backward
    nonincremental-search-forward
    number-to-register
    point-to-register
    prefer-coding-system
    prepend-to-buffer
    prepend-to-register
    read-abbrev-file
    recover-file
    rename-buffer
    run-at-time
    run-with-timer
    search-backward
    search-forward
    select-frame-by-name
    set-background-color
    set-border-color
    set-buffer-file-coding-system
    set-buffer-process-coding-system
    set-cursor-color
    set-foreground-color
    set-frame-font
    set-frame-name
    set-left-margin
    set-mouse-color
    set-right-margin
    set-selection-coding-system
    set-visited-file-name
    sort-regexp-fields
    string-rectangle
    switch-to-buffer-other-frame
    switch-to-buffer-other-window
    vc-comment-search-forward
    vc-comment-search-reverse
    vc-directory
    vc-rename-file
    vc-version-diff
    vc-version-other-window
    view-register
    window-configuration-to-register
    zap-to-char
    )
  "Precomputed list of interactive functions that have to be fixed.
Precomputing this saves time at start-up.")

;;}}}
(defcustom emacspeak-play-emacspeak-startup-icon t
  "If set to T, emacspeak plays its icon as it launches."
  :type 'boolean
  :group 'emacspeak)
(defvar emacspeak-unibyte t
  "Set this to nil before starting  emacspeak 
if you are running in a multibyte enabled environment.")


(defun emacspeak()
  "Starts the Emacspeak speech subsystem.  Use emacs as you
normally would, emacspeak will provide you spoken feedback
as you work.  Emacspeak also provides commands for having
parts of the current buffer, the mode-line etc to be spoken.

If you are hearing this description as a result of pressing
\\[emacspeak-describe-emacspeak] you may want to press
\\[dtk-stop] to stop speech, and then use the arrow keys to
move around in the Help buffer to read the rest of this
description, which includes a summary of all emacspeak
keybindings.

All emacspeak commands use \\[emacspeak-prefix-command] as a
prefix key.  You can also set the state of the TTS engine  by
using \\[emacspeak-dtk-submap-command] as a prefix.  Here is
a summary of all emacspeak commands along with their
bindings.  You need to precede the keystrokes listed below
with \\[emacspeak-prefix-command].

Emacspeak also provides a fluent speech extension to the
emacs terminal emulator (eterm).  Note: You need to use the
term package that comes with emacs-19.29 and later.

\\{emacspeak-keymap}

See the online documentation for individual commands and
functions for details.   "
  (interactive)
  (declare (special mark-even-if-inactive
                    default-enable-multibyte-characters
                    emacspeak-unibyte
                    emacspeak-play-program
                    emacspeak-sounds-directory
                    emacspeak-emacs-commands-to-fix))
  ;;; fixes transient mark mode in emacspeak 
  (setq mark-even-if-inactive t)
  ;;; force unibyte
  (when emacspeak-unibyte
    (setq default-enable-multibyte-characters nil))
  (emacspeak-export-environment)
  (require 'dtk-speak)
  (dtk-initialize)
  (require 'emacspeak-speak)
  (require 'emacspeak-redefine)
  (require 'emacspeak-fix-interactive)
  (require 'emacspeak-keymap)
  (require 'emacspeak-advice)
  (require 'emacspeak-replace)
  (require 'emacspeak-buff-menu)
  (when (and  emacspeak-play-emacspeak-startup-icon 
              (file-exists-p "/usr/bin/mpg123"))
    (start-process "mp3" nil "mpg123"
                   "-q"
                   (expand-file-name "emacspeak.mp3" emacspeak-sounds-directory)))
  (emacspeak-sounds-define-theme-if-necessary emacspeak-sounds-default-theme)
  (mapcar 'emacspeak-fix-interactive-command-if-necessary
          emacspeak-emacs-commands-to-fix)
  (run-hooks 'emacspeak-startup-hook)
  (emacspeak-dtk-sync)
  (emacspeak-setup-programming-modes)
  (require 'emacspeak-wizards)
  (message
   (format "  Press %s to get an   overview of emacspeak  %s \
 I am  completely operational,  and all my circuits are functioning perfectly! "
           (substitute-command-keys
            "\\[emacspeak-describe-emacspeak]" )
           emacspeak-version)))

(defun emacspeak-describe-emacspeak ()
  "Give a brief overview of emacspeak."
  (interactive)
  (describe-function 'emacspeak)
  (switch-to-buffer "*Help*")
  (dtk-set-punctuations "all")
  (emacspeak-speak-buffer))

;;}}}
;;{{{ autoloads
(autoload 'emacspeak-daisy-open-book "emacspeak-daisy"
  "Digital Talking Books on the Emacspeak Desktop."
  t)
(autoload 'emacspeak-ocr "emacspeak-ocr"
  "OCR front-end for Emacspeak desktop"
  t)

(autoload 'emacspeak-gridtext "emacspeak-gridtext"
  "Utilities for gridtext " t)

(autoload 'emacspeak-gridtext-load "emacspeak-gridtext"
  "Utilities for gridtext " t)
(autoload 'emacspeak-filtertext "emacspeak-filtertext"
  "Utilities for filtering text." t)
(autoload 'emacspeak-url-template-load
  "emacspeak-url-template"
  "URL Template utility. " t)
(autoload 'emacspeak-url-template-fetch
  "emacspeak-url-template"
  "URL Template utility. " t)
(autoload 'emacspeak-xml-shell "emacspeak-xml-shell"
  "Emacspeak XML Browser." t)
(autoload 'emacspeak-imcom "emacspeak-imcom"
  "Emacs interface to Jabber via IMCOM" t)
(autoload 'emacspeak-freeamp-prefix-command "emacspeak-freeamp"
  "Emacs interface to freeamp" t)
(autoload 'emacspeak-freeamp "emacspeak-freeamp"
  "Emacs interface to freeamp" t)
(autoload 'emacspeak-freeamp-mode  "emacspeak-freeamp"
  "Emacs interface to freeamp" t)
(autoload 'cd-tool "cd-tool" 
  "Play music CDs from Emacs" t)

(mapcar 
 (function
  (lambda (f)
    (autoload  f  "emacspeak-aumix"
      "Setup audio device characteristics" t)))
 (list
  'emacspeak-aumix
  'emacspeak-aumix-reset
  'emacspeak-aumix-volume-decrease
  'emacspeak-aumix-volume-increase 
  'emacspeak-aumix-wave-decrease 
  'emacspeak-aumix-wave-increase ))
(autoload 'emacspeak-websearch-dispatch
  "emacspeak-websearch"
  "Perform a  websearch" t)
(autoload 'emacspeak-websearch-usenet
  "emacspeak-websearch"
  "Browse Usenet Via Dejanews" t)
(autoload 'emacspeak-websearch-emacspeak-archive
  "emacspeak-websearch"
  "Perform a simple Altavista search" t)

(autoload 'emacspeak-forms-find-file "emacspeak-forms"
  "Visit a forms file" t)

(autoload 'emacspeak-remote-connect-to-server "emacspeak-remote"
  "Connect to a remote speech server.
Use this when you are running emacspeak on a remote machine and want
to have speech output on the local desktop.
Unlike the simpler rsh based remote-tcl solution
(see file remote-tcl in the emacspeak distribution)
this command emacspeak-remote-connect-to-server allows you
to get remote speech feedback in cases where the remote machine cannot
use rsh on your local desktop."
  t)

(autoload 'emacspeak-remote-quick-connect-to-server "emacspeak-remote"
  "Connect to a remote speech server.
Use this when you are running emacspeak on a remote machine and want
to have speech output on the local desktop.
Unlike the simpler rsh based remote-tcl solution
(see file remote-tcl in the emacspeak distribution)
this command emacspeak-remote-connect-to-server allows you
to get remote speech feedback in cases where the remote machine cannot
use rsh on your local desktop."
  t)

(autoload 'emacspeak-eterm-remote-term "emacspeak-eterm"
  "Create a terminal to rlogin into a remote host" t)

(autoload 'voice-lock-mode "voice-lock" "voice lock mode" t)

(autoload 'emacspeak-toggle-auditory-icons "emacspeak-sounds"
  "Ask emacspeak to use auditory icons" t)

(autoload 'emacspeak-tabulate-region "emacspeak-tabulate"
  "Identify columns in the region."  t)
    
(autoload 'emacspeak-table-find-csv-file "emacspeak-table-ui"
  "Browse tables. Beginners: please do C-e C-t and specify one of the
.tab files in the tables subdirectory of emacspeak.
Describe function of emacspeak-table-find-file will then give you
detailed documentation on the table browser" t )
(autoload 'emacspeak-table-find-file "emacspeak-table-ui"
  "Browse tables. Beginners: please do C-e C-t and specify one of the
.tab files in the tables subdirectory of emacspeak.
Describe function of emacspeak-table-find-file will then give you
detailed documentation on the table browser" t )

(autoload 'emacspeak-table-display-table-in-region "emacspeak-table-ui"
  "Parse contents of region as tabular data and display it in table
browsing mode.  Beginners: please do C-e C-t and specify one of the
.tab files in the tables subdirectory of emacspeak.  Describe function
of emacspeak-table-find-file will then give you detailed documentation
on the table browser"
  t )

(autoload 'emacspeak-tapestry-describe-tapestry  "emacspeak-tapestry"
  "Describe layout of buffers in current frame.
This needs the tapestry.el package used by many Emacs systems like the
vm mail reader." t)

(autoload 'emacspeak-hide-or-expose-block "emacspeak-hide"
  "Hide or expose blocks of text that share a common prefix.
Useful in reading email, block comments in program source etc." t)

(autoload 'emacspeak-hide-or-expose-all-blocks "emacspeak-hide"
  "Hide or expose blocks of text that share a common prefix.
Useful in reading email, block comments in program source etc." t)

(autoload 'emacspeak-hide-speak-block-sans-prefix "emacspeak-hide"
  "Speak a block of text sans the prefix that appears on every line of
the block. Use in conjunction with emacspeak-hide-or-expose-block"
  t)


(autoload 'emacspeak-realaudio "emacspeak-realaudio"
  "Single click interface to RealAudio" t)
(autoload 'emacspeak-realaudio-browse "emacspeak-realaudio"
  "Single click interface to RealAudio" t)

(autoload 'emacspeak-realaudio-play-url-at-point "emacspeak-realaudio"
  "Single click interface to RealAudio" t)

(autoload 'emacspeak-realaudio-play "emacspeak-realaudio"
  "Single click interface to RealAudio" t)

(autoload 'emacspeak-realaudio-stop "emacspeak-realaudio"
  "Single click interface to RealAudio" t)


;;}}}
;;{{{ Package Setup Helper

(defun emacspeak-do-package-setup (package module)
  "Setup Emacspeak extension for a specific PACKAGE.
This function  adds the appropriate form to
`after-load-alist' to set up Emacspeak support for a given
package.
Argument MODULE specifies the emacspeak module that implements the speech-enabling extensions."
  (declare (special load-history))
  (cond
   ((assoc package load-history)
    (require module)
    (emacspeak-fix-commands-loaded-from package))
   (t
    (add-hook 'after-load-alist
              (`
               ((, package)
                (progn
                  (require (quote (, module )))
                  (emacspeak-fix-commands-loaded-from (, package)))))))))

;;}}}
;;{{{ Setup package extensions
(emacspeak-do-package-setup "analog" 'emacspeak-analog)
(emacspeak-do-package-setup "ansi-color" 'emacspeak-ansi-color)                            
(emacspeak-do-package-setup "arc-mode" 'emacspeak-arc)
(emacspeak-do-package-setup "babel" 'emacspeak-babel )
(emacspeak-do-package-setup "bbdb" 'emacspeak-bbdb )
(emacspeak-do-package-setup "bibtex" 'emacspeak-bibtex)
(emacspeak-do-package-setup "bookmark" 'emacspeak-bookmark)
(emacspeak-do-package-setup "browse-kill-ring" 'emacspeak-browse-kill-ring )
(emacspeak-do-package-setup "buff-sel" 'emacspeak-buff-sel)
(emacspeak-do-package-setup "bs" 'emacspeak-bs)
(emacspeak-do-package-setup "c-mode" 'emacspeak-c)
(emacspeak-do-package-setup "calc" 'emacspeak-calc)
(emacspeak-do-package-setup "calculator" 'emacspeak-calculator)
(emacspeak-do-package-setup "calendar" 'emacspeak-calendar)
(emacspeak-do-package-setup "cc-mode" 'emacspeak-c)
(emacspeak-do-package-setup "checkdoc" 'emacspeak-checkdoc)
(emacspeak-do-package-setup "cmuscheme" 'emacspeak-cmuscheme)
(emacspeak-do-package-setup "compile" 'emacspeak-compile)
(emacspeak-do-package-setup "cperl-mode" 'emacspeak-cperl)
(emacspeak-do-package-setup "ecb" 'emacspeak-ecb)
(emacspeak-do-package-setup "cus-edit" 'emacspeak-custom)
(emacspeak-do-package-setup "dired" 'emacspeak-dired )
(emacspeak-do-package-setup "dismal" 'emacspeak-dismal)
(emacspeak-do-package-setup "dictation"
                            'emacspeak-dictation)
(emacspeak-do-package-setup "dictionary" 'emacspeak-dictionary)
(emacspeak-do-package-setup "dmacro" 'emacspeak-dmacro)
(emacspeak-do-package-setup "doctor" 'emacspeak-entertain)
(emacspeak-do-package-setup "dunnet" 'emacspeak-entertain)
(emacspeak-do-package-setup "ediff" 'emacspeak-ediff)
(emacspeak-do-package-setup "eperiodic" 'emacspeak-eperiodic)
(emacspeak-do-package-setup "erc" 'emacspeak-erc)
(emacspeak-do-package-setup "eshell" 'emacspeak-eshell)
(emacspeak-do-package-setup "enriched" 'emacspeak-enriched)
(emacspeak-do-package-setup "facemenu" 'emacspeak-facemenu)
(emacspeak-do-package-setup "find-dired" 'emacspeak-find-dired)
(emacspeak-do-package-setup "find-func" 'emacspeak-find-func)
(emacspeak-do-package-setup "flyspell" 'emacspeak-flyspell)
(emacspeak-do-package-setup "folding" 'emacspeak-folding)
(emacspeak-do-package-setup "forms" 'emacspeak-forms)
(emacspeak-do-package-setup "generic" 'emacspeak-generic)
(emacspeak-do-package-setup "gnus" 'emacspeak-gnus)
(emacspeak-do-package-setup "gnuplot" 'emacspeak-gnuplot)
(emacspeak-do-package-setup "gomoku" 'emacspeak-gomoku)
(emacspeak-do-package-setup "gud" 'emacspeak-gud)
(emacspeak-do-package-setup "hangman" 'emacspeak-entertain)
(emacspeak-do-package-setup "hideshow" 'emacspeak-hideshow)
(emacspeak-do-package-setup "html-helper-mode" 'html-voice )
(emacspeak-do-package-setup "hyperbole" 'emacspeak-hyperbole)
(emacspeak-do-package-setup "imenu" 'emacspeak-imenu)
(emacspeak-do-package-setup "ibuffer" 'emacspeak-ibuffer)
(emacspeak-do-package-setup "info" 'emacspeak-info)
(emacspeak-do-package-setup "ispell" 'emacspeak-ispell)
(emacspeak-do-package-setup "jde" 'emacspeak-jde)
(emacspeak-do-package-setup "kotl" 'emacspeak-kotl)
(emacspeak-do-package-setup "make-mode" 'emacspeak-make-mode)
(emacspeak-do-package-setup "man" 'emacspeak-man)
(emacspeak-do-package-setup "message" 'emacspeak-message)
(emacspeak-do-package-setup "meta-mode" 'emacspeak-metapost)
(emacspeak-do-package-setup "mpg123" 'emacspeak-mpg123)
(emacspeak-do-package-setup "midge-mode" 'emacspeak-midge)
(emacspeak-do-package-setup "mpuz" 'emacspeak-entertain)
(emacspeak-do-package-setup "mspools" 'emacspeak-mspools)
(emacspeak-do-package-setup "net-utils" 'emacspeak-net-utils)
(emacspeak-do-package-setup "oobr" 'emacspeak-oo-browser)
(emacspeak-do-package-setup "outline" 'emacspeak-outline)
(emacspeak-do-package-setup "perl-mode" 'emacspeak-perl)
(emacspeak-do-package-setup "pcl-cvs" 'emacspeak-pcl-cvs)
(emacspeak-do-package-setup "psgml" 'emacspeak-psgml)
(emacspeak-do-package-setup "python-mode" 'emacspeak-python)
(emacspeak-do-package-setup "reftex" 'emacspeak-reftex)
(emacspeak-do-package-setup "rmail" 'emacspeak-rmail)
(emacspeak-do-package-setup "sgml-mode"
                            'emacspeak-sgml-mode)
(emacspeak-do-package-setup "sh-script" 'emacspeak-sh-script)
(emacspeak-do-package-setup "solitaire" 'emacspeak-solitaire)
(emacspeak-do-package-setup "speedbar" 'emacspeak-speedbar)
(emacspeak-do-package-setup "sawfish" 'emacspeak-sawfish)
(emacspeak-do-package-setup "sql-mode" 'emacspeak-sql)
(emacspeak-do-package-setup "sql" 'emacspeak-sql)
(emacspeak-do-package-setup "supercite" 'emacspeak-supercite)
(emacspeak-do-package-setup "tar-mode" 'emacspeak-tar)
(emacspeak-do-package-setup "tcl" 'emacspeak-tcl)
(emacspeak-do-package-setup "tdtd" 'emacspeak-tdtd)
(emacspeak-do-package-setup "xslide" 'emacspeak-xslide)
(emacspeak-do-package-setup "xslt-process" 'emacspeak-xslt-process)
(emacspeak-do-package-setup "tempo" 'emacspeak-tempo)
(emacspeak-do-package-setup "tnt" 'emacspeak-tnt)
(emacspeak-do-package-setup "term" 'emacspeak-eterm )
(emacspeak-do-package-setup "eudc" 'emacspeak-eudc )
(emacspeak-do-package-setup "tetris" 'emacspeak-tetris)
(emacspeak-do-package-setup "tex-site" 'emacspeak-auctex)
(emacspeak-do-package-setup "texinfo" 'emacspeak-texinfo)
(emacspeak-do-package-setup "tmm" 'emacspeak-facemenu)
(emacspeak-do-package-setup "view" 'emacspeak-view)
(emacspeak-do-package-setup "view-process-mode" 'emacspeak-view-process)
(emacspeak-do-package-setup "vm" 'emacspeak-vm)
(emacspeak-do-package-setup "w3" 'emacspeak-w3)
(emacspeak-do-package-setup "w3m" 'emacspeak-w3m)
(emacspeak-do-package-setup "wid-edit" 'emacspeak-widget)
(emacspeak-do-package-setup "windmove" 'emacspeak-windmove)
(emacspeak-do-package-setup "winring" 'emacspeak-winring)
(emacspeak-do-package-setup "wrolo" 'emacspeak-wrolo)
                                                                                          
;;}}}

;;{{{ finder

;;; Finder is special -- it needs to conditionally
;;; regenerate the database
(add-hook 'after-load-alist
          '("finder"
            (progn
              (load-library "emacspeak-finder")
              (unless (file-newer-than-file-p
                       (concat emacspeak-lisp-directory "/"
                               "emacspeak-finder-inf.el")
                       (concat emacspeak-lisp-directory "/" "emacspeak.el"))
                (emacspeak-finder-compile-keywords))
              (load-library "emacspeak-finder-inf")
              (push
               (cons 'emacspeak "Audio Desktop")
               finder-known-keywords))))

;;}}}
;;{{{  Submit bugs

(defconst emacspeak-bug-address
  "raman@cs.cornell.edu"
  "Address of the maintainer of this package.")


(defun emacspeak-submit-bug ()
  "Function to submit a bug to the programs maintainer."
  (interactive)
  (require 'reporter)
  (when
      (yes-or-no-p "Are you sure you want to submit a bug report? ")
    (let (
          (vars '(window-system
                  window-system-version
                  emacs-version
                  system-type
                  emacspeak-version dtk-tcl dtk-program
                  dtk-speech-rate dtk-character-scale
                  dtk-split-caps dtk-capitalize
                  dtk-punctuation-mode
                  emacspeak-line-echo  emacspeak-word-echo
                  emacspeak-character-echo
                  emacspeak-use-auditory-icons
                  emacspeak-audio-indentation )))
      (mapcar
       (function
        (lambda (x)
          (if (not (and (boundp x) (symbol-value x)))
              (setq vars (delq x vars))))) vars)
      (reporter-submit-bug-report  emacspeak-bug-address
                                   (concat "Emacspeak Version: " emacspeak-version )
                                   vars
                                   nil nil
                                   "Description of Problem:"))))

;;}}}
;;{{{ exporting emacspeak environment to subprocesses

(defun emacspeak-export-environment ()
  "Export shell environment.
This exports emacspeak's system variables to the environment
so it can be passed to subprocesses.
Additionally, we set EMACS_UNIBYTE to avoid problems under
Emacs 20.3"
  (declare (special emacspeak-directory
                    emacspeak-play-program
                    emacspeak-sounds-directory
                    emacspeak-unibyte))
  (when emacspeak-unibyte
    (setenv "EMACS_UNIBYTE" "1"))
  (setenv "EMACSPEAK_DIR" emacspeak-directory)
  (setenv "EMACSPEAK_SOUNDS_DIR" emacspeak-sounds-directory)
  (setenv "EMACSPEAK_PLAY_PROGRAM" emacspeak-play-program)
  )

;;}}}
;;{{{  personal keymaps 

(defun emacspeak-personal-keybindings ()
  "Load user's personal keybindings for the audio desktop."
  (declare (special emacspeak-personal-keys))
  (when (locate-library "emacspeak-personal")
    (load-library "emacspeak-personal")
    (when emacspeak-personal-keymap
      (setq emacspeak-personal-keys 
            (mapcar
             (lambda (binding)
               (cond
                ((numberp (car binding))
                 (cons (format "%c" (car binding))
                       (cdr binding)))
                (t binding)))
             (cdr emacspeak-personal-keymap))))))

(add-hook 'emacspeak-startup-hook 'emacspeak-personal-keybindings)

;;}}}
;;{{{ setup programming modes 

;;; turn on automatic voice locking , split caps and punctuations for programming modes


(defun emacspeak-setup-programming-mode ()
  "Setup programming mode. Turns on audio indentation and
sets punctuation mode to all, and turns on split caps."
  (declare (special dtk-split-caps
                    emacspeak-audio-indentation))
  (voice-lock-mode 1)
  (dtk-set-punctuations "all")
  (or dtk-split-caps
      (dtk-toggle-split-caps))
  (or emacspeak-audio-indentation
      (emacspeak-toggle-audio-indentation))
  (emacspeak-dtk-sync))

(defun emacspeak-setup-programming-modes ()
  "Setup programming modes."
  (mapcar
   (function (lambda (hook)
               (add-hook hook
                         'emacspeak-setup-programming-mode)))
   (list 'c-mode-common-hook
         'py-mode-hook
         'lisp-mode-hook
         'emacs-lisp-mode-hook
         'lisp-interaction-mode-hook
         'midge-mode-hook
         'meta-common-mode-hook
         'perl-mode-hook
         'cperl-mode-hook
         'sh-mode-hook
         'sql-mode-hook
         'sgml-mode-hook
         'makefile-mode-hook
         'tex-mode-hook
         'tcl-mode-hook
         'html-helper-mode-hook
         'scheme-mode-hook
         'dired-mode-hook)))

;;}}}
;;{{{ set up after-init-hook to fix interactive functions 

(add-hook 'after-init-hook
          'emacspeak-fix-commands-that-use-interactive)

;;}}}
(provide 'emacspeak)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
