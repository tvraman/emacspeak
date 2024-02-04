;;; emacspeak.el --- The Complete Audio Desktop  -*- lexical-binding: t; -*-
;;
;; $Author: tv.raman.tv $
;; Description:  Emacspeak: A speech interface to Emacs
;; Keywords: Emacspeak, Speech, Dectalk,
;; Version: 55.0
;; Package-Requires: ((emacs "26") (hydra "0.5"))
;;;   LCD Archive entry:
;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;;
;;  $Revision: 4642 $ |
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

;; Emacspeak extends Emacs to be a fully functional audio desktop.
;; This is the main emacspeak module.
;; It actually does very little:
;; @itemize
;; @item  It sets up Emacs to load package-specific
;; Emacspeak modules as each package is loaded.
;; @item  It implements function emacspeak which loads the rest of the system.
;; @end itemize
;;; Code:

;;;  Required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;   Customize groups

(defgroup emacspeak nil
  "Emacspeak: The Complete Audio Desktop."
  :link '(url-link :tag "Web" "http://emacspeak.sf.net"
                   :help-echo "Emacspeak  Site")
  :link '(url-link :tag "Blog" "http://emacspeak.blogspot.com"
                   :help-echo "Emacspeak Blog")
  :link '(url-link :tag "Apps"
                   "https://tvraman.github.io/emacspeak/applications.html"
                   :help-echo "Browse Speech-Enabled  applications on
the Emacspeak desktop.")
  :link '(url-link :tag "Guide"
                   "https://tvraman.github.io/emacspeak/manual"
                   :help-echo "online user guide.")
  :link '(url-link :tag "Tips"
                   "https://tvraman.github.io/emacspeak/tips.html"
                   :help-echo "Emacspeak Tips and Tricks.")
  :link '(url-link :tag "Mail Archive"
                   "https://www.emacspeak.net")
  ;; end links
  :group 'applications)

;;;  Package Setup Helper

;; This function adds the appropriate form to `after-load-alist' to
;; set up Emacspeak support for a given package.  Argument MODULE (a
;; symbol)specifies the emacspeak module that implements the
;; speech-enabling extensions for `package' (a string).
(defsubst emacspeak-do-package-setup (package module)
  "Setup Emacspeak extension for   PACKAGE by loading MODULE."
  (with-eval-after-load package (require module)))

;; DocView
(declare-function doc-view-open-text "doc-view")

(with-eval-after-load "doc-view"
  (add-hook 'doc-view-mode-hook #'doc-view-open-text))

(with-eval-after-load "gptel"
  (cl-declare (special gptel-post-response-functions))
  (cl-pushnew  'emacspeak-speak-region gptel-post-response-functions))

;;;  Setup package extensions
(defvar emacspeak-packages-to-prepare
  '(
    ("2048-game" emacspeak-2048)
    ("abc-mode" emacspeak-abc-mode)
    ("add-log" emacspeak-add-log)
    ("analog" emacspeak-analog)
    ("annotate" emacspeak-annotate)
    ("apt-sources" emacspeak-apt-sources)
    ("arc-mode" emacspeak-arc)
    ("bbdb" emacspeak-bbdb)
    ("bibtex" emacspeak-bibtex)
    ("bookmark" emacspeak-bookmark)
    ("browse-kill-ring" emacspeak-browse-kill-ring)
    ("bs" emacspeak-bs)
    ("buff-menu" emacspeak-buff-menu)
    ("calc" emacspeak-calc)
    ("calculator" emacspeak-calculator)
    ("calendar" emacspeak-calendar)
    ("calibredb" emacspeak-calibredb)
    ("cc-mode" emacspeak-c)
    ("chess" emacspeak-chess)
    ("cider" emacspeak-cider)
    ("ciel" emacspeak-ciel)
    ("clojure" emacspeak-clojure)
    ("cmuscheme" emacspeak-cmuscheme)
    ("combobulate" emacspeak-combobulate)
    ("comint"  emacspeak-comint)
    ("company" emacspeak-company)
    ("compile" emacspeak-compile)
    ("cperl-mode" emacspeak-cperl)
    ("cus-edit" emacspeak-custom)
    ("deadgrep" emacspeak-deadgrep)
    ("debugger" emacspeak-debugger)
    ("desktop" emacspeak-desktop)
    ("devdocs" emacspeak-devdocs)
    ("dictionary" emacspeak-dictionary)
    ("diff-mode" emacspeak-diff-mode)
    ("dired" emacspeak-dired)
    ("dismal" emacspeak-dismal)
    ("doctor" emacspeak-entertain)
    ("dumb-jump" emacspeak-dumb-jump)
    ("dunnet" emacspeak-entertain)
    ("ecb" emacspeak-ecb)
    ("eat" emacspeak-eat)
    ("ediff" emacspeak-ediff)
    ("eglot" emacspeak-eglot)
    ("ein" emacspeak-ein)
    ("ein-notebook" emacspeak-ein)
    ("elfeed" emacspeak-elfeed)
    ("elisp-refs" emacspeak-elisp-refs)
    ("ellama" emacspeak-ellama)
    ("elpher" emacspeak-elpher)
    ("elpy" emacspeak-elpy)
    ("elscreen" emacspeak-elscreen)
    ("emms" emacspeak-emms)
    ("empv" emacspeak-empv)
    ("enriched" emacspeak-enriched)
    ("enwc" emacspeak-enwc)
    ("epa" emacspeak-epa)
    ("eperiodic" emacspeak-eperiodic)
    ("erc" emacspeak-erc)
    ("eshell" emacspeak-eshell)
    ("ess" emacspeak-ess)
    ("eudc" emacspeak-eudc)
    ("evil" emacspeak-evil)
    ("eww" emacspeak-eww)
    ("exwm" emacspeak-exwm)
    ("ffap" emacspeak-ffap)
    ("find-file-in-project" emacspeak-ffip)
    ("flycheck" emacspeak-flycheck)
    ("flymake" emacspeak-flymake)
    ("flyspell" emacspeak-flyspell)
    ("folding" emacspeak-folding)
    ("forge" emacspeak-forge)
    ("forms" emacspeak-forms)
    ("gdb-ui" emacspeak-gud)
    ("geiser" emacspeak-geiser)
    ("github-explorer" emacspeak-gh-explorer)
    ("gnuplot" emacspeak-gnuplot)
    ("gnus" emacspeak-gnus)
    ("go-mode" emacspeak-go-mode)
    ("gomoku" emacspeak-gomoku)
    ("gtags" emacspeak-gtags)
    ("gud" emacspeak-gud)
    ("hangman" emacspeak-entertain)
    ("haskell-mode" emacspeak-haskell)
    ("helm" emacspeak-helm)
    ("hide-lines" emacspeak-hide-lines)
    ("hideshow" emacspeak-hideshow)
    ("hydra" emacspeak-hydra)
    ("ibuffer" emacspeak-ibuffer)
    ("ido" emacspeak-ido)
    ("iedit" emacspeak-iedit)
    ("indium" emacspeak-indium)
    ("info" emacspeak-info)
    ("ispell" emacspeak-ispell)
    ("ivy" emacspeak-ivy)
    ("jabber" emacspeak-jabber)
    ("jdee" emacspeak-jdee)
    ("journalctl-mode" emacspeak-journalctl)
    ("js2-mode" emacspeak-js2)
    ("kmacro" emacspeak-kmacro)
    ("lispy" emacspeak-lispy)
    ("lua-mode" emacspeak-lua)
    ("magit" emacspeak-magit)
    ("make-mode" emacspeak-make-mode)
    ("man" emacspeak-man)
    ("markdown-mode" emacspeak-markdown)
    ("message" emacspeak-message)
    ("meta-mode" emacspeak-metapost)
    ("midge-mode" emacspeak-midge)
    ("mines" emacspeak-mines)
    ("mpuz" emacspeak-entertain)
    ("mspools" emacspeak-mspools)
    ("muse-mode" emacspeak-muse)
    ("net-utils" emacspeak-net-utils)
    ("newsticker" emacspeak-newsticker)
    ("notmuch" emacspeak-notmuch)
    ("nov" emacspeak-nov)
    ("nxml-mode" emacspeak-nxml)
    ("org" emacspeak-org)
    ("orgalist" emacspeak-orgalist)
    ("origami" emacspeak-origami)
    ("outline" emacspeak-outline)
    ("package"emacspeak-package)
    ("paradox"emacspeak-paradox)
    ("pcvs" emacspeak-pcl-cvs)
    ("perl-mode" emacspeak-perl)
    ("pianobar" emacspeak-pianobar)
    ("pipewire" emacspeak-pipewire)
    ("popup" emacspeak-popup)
    ("proced" emacspeak-proced)
    ("project" emacspeak-project)
    ("projectile" emacspeak-projectile)
    ("pydoc" emacspeak-pydoc)
    ("python" emacspeak-python)
    ("python-mode" emacspeak-py)
    ("racer" emacspeak-racer)
    ("racket-mode" emacspeak-racket)
    ("re-builder" emacspeak-re-builder)
    ("reftex" emacspeak-reftex)
    ("related" emacspeak-related)
    ("rg" emacspeak-rg)
    ("rmail" emacspeak-rmail)
    ("rpm-spec-mode" emacspeak-rpm-spec)
    ("rst" emacspeak-rst)
    ("ruby-mode" emacspeak-ruby)
    ("rust-mode" emacspeak-rust-mode)
    ("sage-shell-mode" emacspeak-sage)
    ("sdcv" emacspeak-sdcv)
    ("selectrum" emacspeak-selectrum)
    ("ses" emacspeak-ses)
    ("sgml-mode" emacspeak-sgml-mode)
    ("sh-script" emacspeak-sh-script)
    ("shx" emacspeak-shx)
    ("slime" emacspeak-slime)
    ("smartparens" emacspeak-smartparens)
    ("solitaire" emacspeak-solitaire)
    ("speedbar" emacspeak-speedbar)
    ("sql" emacspeak-sql)
    ("sudoku" emacspeak-sudoku)
    ("supercite" emacspeak-supercite)
    ("syslog" emacspeak-syslog)
    ("tab-bar" emacspeak-tab-bar)
    ("table" emacspeak-etable)
    ("tabulated-list" emacspeak-tabulated-list)
    ("tar-mode" emacspeak-tar)
    ("tcl" emacspeak-tcl)
    ("tempo" emacspeak-tempo)
    ("term" emacspeak-eterm)
    ("tetris" emacspeak-tetris)
    ("tex-site" emacspeak-auctex)
    ("texinfo" emacspeak-texinfo)
    ("threes" emacspeak-threes)
    ("tide" emacspeak-tide)
    ("todo-mode" emacspeak-todo-mode)
    ("transient" emacspeak-transient)
    ("treesit" emacspeak-treesit)
    ("typo" emacspeak-typo)
    ("vdiff" emacspeak-vdiff)
    ("vertico" emacspeak-vertico)
    ("view" emacspeak-view)
    ("vm" emacspeak-vm)
    ("vterm" emacspeak-vterm)
    ("vuiet" emacspeak-vuiet)
    ("wdired" emacspeak-wdired)
    ("wid-edit" emacspeak-widget)
    ("widget" emacspeak-widget)
    ("windmove" emacspeak-windmove)
    ("winring" emacspeak-winring)
    ("woman" emacspeak-woman)
    ("xkcd" emacspeak-xkcd)
    ("xref" emacspeak-xref)
    ("yaml-mode" emacspeak-yaml)
    ("yasnippet" emacspeak-yasnippet)
    )
  "Packages to  speech-enable.")

(defun emacspeak-prepare-emacs ()
  "Prepare Emacs to speech-enable packages when loaded."
  (cl-declare (special emacspeak-packages-to-prepare
                       Info-file-list-for-emacs
                       emacspeak-soundscapes))
  (push "emacspeak" Info-file-list-for-emacs)
  (setq-default line-move-visual nil)
  (setq use-dialog-box nil)
  (when (boundp 'Info-directory-list)
    (push emacspeak-info-directory Info-directory-list))
  (mapc
   #'(lambda (pair)
       (emacspeak-do-package-setup  (cl-first pair) (cl-second pair)))
   emacspeak-packages-to-prepare)
  (when emacspeak-soundscapes (soundscape-toggle)))

;;;  setup programming modes

;; turn on automatic voice locking , split caps and punctuations in
;; programming  modes

;;;###autoload
(defsubst emacspeak-setup-programming-mode ()
  "Setup programming mode."
  (cl-declare (special dtk-split-caps emacspeak-audio-indentation dtk-caps))
  (ems-with-messages-silenced
    (dtk-set-punctuations 'all)
    (or dtk-split-caps (dtk-toggle-split-caps))
    (or dtk-caps (dtk-toggle-caps))
    (emacspeak-pronounce-refresh-pronunciations)
    (or emacspeak-audio-indentation
        (emacspeak-toggle-audio-indentation))))

(defun emacspeak-setup-programming-modes ()
  "Setup programming modes."
  (add-hook 'prog-mode-hook #'emacspeak-setup-programming-mode)
  (with-eval-after-load "generic-x"
    (mapc
     #'(lambda (hook)
         (add-hook hook #'emacspeak-setup-programming-mode ))
     generic-extras-enable-list))
  (mapc
   #'(lambda (hook)
       (add-hook hook #'emacspeak-setup-programming-mode))
   '(
     conf-unix-mode-hook html-helper-mode-hook
     markdown-mode-hook muse-mode-hook
     sgml-mode-hook xml-mode-hook nxml-mode-hook xsl-mode-hook
     TeX-mode-hook LaTeX-mode-hook bibtex-mode-hook)))

;;;  Emacspeak:

(defvar emacspeak-play-startup-icon t
  "If set to T, emacspeak plays its icon as it launches.
This cannot be set via custom; set this in your startup file before
  you load anything else.")

(defsubst emacspeak-play-startup-icon ()
  "Play startup icon."
  (cl-declare (special emacspeak-play-startup-icon sox-play))
  (when (and  emacspeak-play-startup-icon sox-play)
    (start-process "ogg" nil sox-play emacspeak-icon)))

(defsubst emacspeak-easter-egg ()
  "Easter Egg"
  (cl-declare (special emacspeak-play))
  (let ((f (expand-file-name "ai/01-gemini.ogg" emacspeak-etc-directory)))
    (when
        (and  emacspeak-play-startup-icon sox-play
              (file-exists-p f)
              (string=                  ; anniversary
               (format-time-string "%m-%d") (format-time-string "04-25")))
      (start-process "ogg" nil sox-play f))))

(defvar emacspeak-startup-message
  (eval-when-compile
    (format
     "  Press %s to get an   overview of emacspeak  %s. \
 I am  completely operational,  and all my circuits are functioning perfectly!"
     (substitute-command-keys
      "\\[emacspeak-describe-emacspeak]")
     emacspeak-version))
  "Emacspeak startup message.")

(defcustom emacspeak-soundscapes nil
  "Whether we should turn on soundscapes on startup."
  :type 'boolean
  :group 'emacspeak)

;;;###autoload
(defun emacspeak()
  "Start the Emacspeak Audio Desktop.
Use Emacs as you normally would, emacspeak provides
 spoken feedback.  Emacspeak also provides commands
for having parts of the current buffer, the mode-line etc to be
spoken.

 Emacspeak commands use \\[emacspeak-keymap] as a prefix
key.  You can configure TTS  with
\\[emacspeak-dtk-submap] as a prefix.

\\{emacspeak-keymap}

Emacspeak provides a set of additional keymaps to give easy access to
its extensive facilities.

Press C-; to access keybindings in emacspeak-hyper-keymap:
\\{emacspeak-hyper-keymap}

Press C-' or C-.  to access keybindings in emacspeak-super-keymap:
\\{emacspeak-super-keymap}

Press C-, to access keybindings in emacspeak-alt-keymap:
\\{emacspeak-alt-keymap}

See the online documentation \\[emacspeak-open-info] for individual
commands and options for details."
  (dtk-initialize)
  (setq ring-bell-function #'(lambda nil (emacspeak-auditory-icon 'warn-user)))
  (emacspeak-sounds-select-theme emacspeak-sounds-current-theme)
  (emacspeak-pronounce-load-dictionaries)
  (make-thread #'(lambda () (ems--fastload "emacspeak-advice")))
  (emacspeak-setup-programming-modes)
  (make-thread #'emacspeak-prepare-emacs)
  (setq line-number-mode nil column-number-mode nil)
  (global-visual-line-mode -1)
  (transient-mark-mode -1)
  (add-to-list
   'minor-mode-alist
   '(emacspeak-speak-show-volume (:eval (ems--show-current-volume))))
  (setenv "EMACSPEAK_DIR" emacspeak-directory)
  (message emacspeak-startup-message)
  (emacspeak-play-startup-icon)
  (emacspeak-easter-egg))

(provide 'emacspeak)
;;; Orca For Lock Screen:

;; Orca Toggle:
;; Easily start/stop orca for use with lock-screen, Chrome etc.

(defvar emacspeak-orca-handle nil
  "Orca process handle")
;;;###autoload
(defun emacspeak-orca-toggle ()
  "Toggle state of orca."
  (interactive)
  (cl-declare (special emacspeak-orca-handle))
  (cond
   (emacspeak-orca-handle (delete-process emacspeak-orca-handle)
                          (setq emacspeak-orca-handle  nil))
   (t (setq emacspeak-orca-handle (start-process "Orca"nil "orca")))))
;;;  end of file

;;; emacspeak.el ends here
