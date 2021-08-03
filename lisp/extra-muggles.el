;;; extra-muggles.el --- Additional Hydras For The Emacspeak Desktop  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MUGGLES An Emacs Interface to muggles
;;; Keywords: Emacspeak,  Audio Desktop muggles
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
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
;;; MERCHANTABILITY or FITNMUGGLES FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; MUGGLES ==  Emacspeak spells for power-users.
;;; These are extra hydras that I dont use very often,
;;; And are being moved here from emacspeak-muggles to save time at
;;; startup.
;;;
;;; This module implements no new functionality --- contrast with
;;; emacspeak-wizards.  Instead, it uses package hydra to provide
;;; convenience key-bindings that access existing Emacspeak
;;; functionality.
;;; You need to install package Hydra first:
;;; @samp{M-x package-install  hydra}.

;;; Note that on newer versions of Emacs, loading this module will
;;; attempt to automatically install package hydra if it is not found.
;;; @subsection Using Hydras
;;; See the high-level documentation for the Hydra package by executing
;;; @kbd {C-h C-j } hydra @kbd{RET}.
;;; The documentation in this section uses the same terminology as
;;; shown in the hydra package documentation.

;;; @subsection Extra Muggles

;;;@itemize
;;; @item View-Mode: @kbd{C-. v} Temporarily behave like view-mode.
;;; @item SmartParens: @kbd{C-c ,} Smart Parens
;;;@item m-player: @kbd{s-m} Emacspeak-M-Player Commands
;;;@item m-player: @kbd{s-;} Emacspeak-M-Player muggle
;;;@item pianobar: @kbd{s-'} Emacspeak-M-pianobar Commands
;;; @item origami: C-, / Origami   bindings.
;;; @item outliner: <C-c .> Bindings from outline-minor-mode.
;;;@item Info-Summary: <?> in Info Info Summary Muggle
;;; @item Vuiet Explorer: @kbd{C-; v} Vuiet Music Explorer and Player
;;;@end itemize

;;; Emacspeak automatically speaks Hydra hints when displayed.
;;; To silence all Hydra hints, set hydra-is-helpful to nil.  To
;;; temporarily silence speaking of Hydra hints, Muggles can bind
;;; command @code{emacspeak-hydra-toggle-talkative}.  As an
;;; example, Muggle @samp{ViewMode} binds @code{s} to this command.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'emacspeak-dired)
(eval-when-compile
  (when (locate-library "package")
    (unless (locate-library "hydra") (package-install 'hydra)))
  (require 'emacspeak-hydra)
  (require 'emacspeak-outline)
  (require 'vuiet nil 'no-error)
  (require 'smartparens "smartparens" 'no-error)
  (require 'hydra "hydra" 'no-error)
  (require 'view)
  (require 'emacspeak-m-player))
(with-no-warnings (require 'origami "origami" 'no-error))


;;}}}
;;{{{ Generate Muggles From Keymaps:

;;; Generate A Muggle:
;;; Take a name of a keymap (symbol)
;;; And generate an interactive command that can be bound to a key.
;;; Invoking that command temporarily activates the previously supplied keymap.
;;; That activated keymap remains active until the user presses a key that is not bound in that keymap.
;;; Inspired by the Hydra package.

(defun emacspeak-muggles-generate (k-map)
  "Generate a Muggle from specified k-map.
Argument `k-map' is a symbol  that names a keymap."
  (unless (and (symbolp k-map) (boundp k-map) (keymapp (symbol-value k-map)))
    (error "%s is not a keymap." k-map))
  (let ((cmd-name (intern (format "emacspeak-muggles-%s-cmd" k-map)))
        (doc-string (format "Temporarily use keymap %s" k-map)))
    (eval
     `(defun ,cmd-name ()
        ,doc-string
        (interactive)
        (let* ((key (read-key-sequence "Key: "))
               (cmd (lookup-key ,k-map key)))
          (while (commandp cmd)
            (call-interactively cmd)
            (setq key (read-key-sequence "Key: ")
                  cmd (lookup-key ,k-map key)))
          (call-interactively (lookup-key (current-global-map) key))
          (emacspeak-auditory-icon 'close-object))))))

;;; Create a command to invoke our media player map:

(global-set-key
 (ems-kbd "s-m")
 (emacspeak-muggles-generate 'emacspeak-m-player-mode-map))

;; Create one for pianobar
(when (featurep 'pianobar)
  (global-set-key
   (ems-kbd "s-'")
   (emacspeak-muggles-generate 'pianobar-key-map)))

;;}}}
;;{{{ Media Player:

(declare-function emacspeak-amark-save "emacspeak-muggles" t)
(global-set-key
 (ems-kbd "s-;")
 (defhydra emacspeak-muggles-m-player
   (:body-pre (emacspeak-hydra-body-pre "Media Player")
              :pre emacspeak-hydra-pre :post emacspeak-hydra-post)
   (";" emacspeak-m-player)
   ("+" emacspeak-m-player-volume-up)
   ("," emacspeak-m-player-backward-10s)
   ("%" emacspeak-m-player-display-percent)
   ("-" emacspeak-m-player-volume-down)
   ("." emacspeak-m-player-forward-10s)
   ("<" emacspeak-m-player-backward-1min)
   ("<down>" emacspeak-m-player-forward-1min)
   ("<end>" emacspeak-m-player-end-of-track)
   ("<home>" emacspeak-m-player-beginning-of-track)
   ("<left>" emacspeak-m-player-backward-10s)
   ("<next>" emacspeak-m-player-forward-10min)
   ("<prior>" emacspeak-m-player-backward-10min)
   ("<right>" emacspeak-m-player-forward-10s)
   ("<up>" emacspeak-m-player-backward-1min)
   ("=" emacspeak-m-player-volume-up)
   (">" emacspeak-m-player-forward-1min)
   ("?" emacspeak-m-player-display-position)
   ("C" emacspeak-m-player-clear-filters)
   ("C-m" emacspeak-m-player-load)
   ("DEL" emacspeak-m-player-reset-speed)
   ("M-l" emacspeak-m-player-load-playlist)
   ("O" emacspeak-m-player-reset-options)
   ("P" emacspeak-m-player-apply-reverb-preset)
   ("Q" emacspeak-m-player-quit "quit")
   ("R" emacspeak-m-player-edit-reverb)
   ("S" emacspeak-amark-save)
   ("SPC" emacspeak-m-player-pause)
   ("[" emacspeak-m-player-slower)
   ("]" emacspeak-m-player-faster)
   ("a" emacspeak-m-player-amark-add)
   ("b" emacspeak-m-player-balance)
   ("c" emacspeak-m-player-slave-command)
   ("d" emacspeak-m-player-delete-filter)
   ("e" emacspeak-m-player-add-equalizer)
   ("f" emacspeak-m-player-add-filter)
   ("g" emacspeak-m-player-seek-absolute)
   ("j" emacspeak-m-player-amark-jump)
   ("l" emacspeak-m-player-get-length)
   ("m" emacspeak-m-player-mode-line)
   ("n" emacspeak-m-player-next-track)
   ("o" emacspeak-m-player-customize-options)
   ("p" emacspeak-m-player-previous-track)
   ("q" bury-buffer)
   ("r" emacspeak-m-player-seek-relative)
   ("s" emacspeak-m-player-scale-speed)
   ("t" emacspeak-m-player-play-tracks-jump)
   ("u" emacspeak-m-player-url)
   ("v" emacspeak-m-player-volume-change)
   ("(" emacspeak-m-player-left-channel)
   (")" emacspeak-m-player-right-channel)
   ("{" emacspeak-m-player-half-speed)
   ("}" emacspeak-m-player-double-speed)
   ))

;;}}}
;;{{{ Outliner:

;;; Cloned from Hydra Wiki:
(global-set-key
 (ems-kbd "C-. o")
 (defhydra emacspeak-muggles-outliner
   (:body-pre
    (progn
      (outline-minor-mode 1)
      (emacspeak-hydra-body-pre "Outline Navigation"))
    :pre emacspeak-hydra-pre :post emacspeak-hydra-post
    :color pink :hint nil)
   "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
   ("?" (emacspeak-hydra-self-help "emacspeak-muggles-outliner"))
   ;; Hide
   ("q" outline-hide-sublevels) ; Hide everything but the top-level headings
   ("t" outline-hide-body) ; Hide everything but headings (all body lines)
   ("o" outline-hide-other)             ; Hide other branches
   ("c" outline-hide-entry)             ; Hide this entry's body
   ("l" outline-hide-leaves) ; Hide body lines in this entry and sub-entries
   ("d" outline-hide-subtree) ; Hide everything in this entry and sub-entries
   ;; Show
   ("a" outline-show-all)               ; Show (expand) everything
   ("e" outline-show-entry)             ; Show this heading's body
   ("i" outline-show-children) ; Show this heading's immediate child sub-headings
   ("k" outline-show-branches) ; Show all sub-headings under this heading
   ("s" outline-show-subtree) ; Show (expand) everything in this heading & below
   ;; Move
   ("u" outline-up-heading)               ; Up
   ("n" outline-next-visible-heading)     ; Next
   ("p" outline-previous-visible-heading) ; Previous
   ("f" outline-forward-same-level)       ; Forward - same level
   ("b" outline-backward-same-level)      ; Backward - same level
   ("z" nil "leave")))

;;}}}
;;{{{ Info Summary:

;;; Taken from Hydra wiki and customized to taste:
(define-key Info-mode-map (ems-kbd "?")
  (defhydra emacspeak-muggles-info-summary
    (:color blue :hint nil
            :body-pre (emacspeak-hydra-body-pre "Info Summary")
            :pre emacspeak-hydra-pre :post emacspeak-hydra-post)
    "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
    ("]"   Info-forward-node)
    ("["   Info-backward-node)
    ("n"   Info-next)
    ("p"   Info-prev)
    ("s"   Info-search)
    ("S"   Info-search-case-sensitively)

    ("l"   Info-history-back)
    ("r"   Info-history-forward)
    ("H"   Info-history)
    ("t"   Info-top-node)
    ("<"   Info-top-node)
    (">"   Info-final-node)

    ("u"   Info-up)
    ("^"   Info-up)
    ("m"   Info-menu)
    ("g"   Info-goto-node)
    ("b"   beginning-of-buffer)
    ("e"   end-of-buffer)

    ("f"   Info-follow-reference)
    ("i"   Info-index)
    (","   Info-index-next)
    ("I"   Info-virtual-index)

    ("T"   Info-toc)
    ("d"   Info-directory)
    ("c"   Info-copy-current-node-name)
    ("C"   clone-buffer)
    ("a"   info-apropos)

    ("1"   Info-nth-menu-item)
    ("2"   Info-nth-menu-item)
    ("3"   Info-nth-menu-item)
    ("4"   Info-nth-menu-item)
    ("5"   Info-nth-menu-item)
    ("6"   Info-nth-menu-item)
    ("7"   Info-nth-menu-item)
    ("8"   Info-nth-menu-item)
    ("9"   Info-nth-menu-item)

    ("?"   Info-summary "Info summary")
    ("h"   Info-help "Info help")
    ("q"   quit-window "Info exit")
    ("C-g" nil "cancel" :color blue)))

;;}}}
;;{{{ origami:

(declare-function origami-open-node "origami" (buffer point))
(declare-function origami-close-node "origami" (buffer point))
(declare-function origami-next-fold "origami" (buffer point))
(declare-function origami-previous-fold "origami" (buffer point))
(declare-function origami-forward-toggle-node "origami" (buffer point))
(declare-function origami-toggle-all-nodes "origami" (buffer))
(declare-function origami-mode "origami" (&optional arg))

(global-set-key
 (ems-kbd "C-, /")
 (defhydra emacspeak-origami
   (:color red
           :body-pre
           (progn
             (origami-mode 1)
             (emacspeak-hydra-body-pre "Origami")
             (emacspeak-hydra-toggle-talkative))
           :hint nil
           :pre emacspeak-hydra-pre :post emacspeak-hydra-post)
   "
    _o_pen node    _n_ext fold       toggle _f_orward
    _c_lose node   _p_revious fold   toggle _a_ll
    "
   ("o" origami-open-node)
   ("c" origami-close-node)
   ("n" origami-next-fold)
   ("p" origami-previous-fold)
   ("f" origami-forward-toggle-node)
   ("a" origami-toggle-all-nodes)))

;;}}}
;;{{{ Muggles Autoload Wizard:

(defvar emacspeak-muggles-pattern
  "emacspeak-muggles-.*/body$"
  "Pattern matching muggles we are interested in.")

(defun emacspeak-muggles-enumerate ()
  "Enumerate all interactive muggles."
  (cl-declare (special emacspeak-muggles-pattern))
  (let ((result nil))
    (mapatoms
     #'(lambda (s)
         (let ((name (symbol-name s)))
           (when
               (and
                (commandp s)
                (string-match emacspeak-muggles-pattern  name))
             (push s result)))))
    result))


(defun emacspeak-muggles-generate-autoloads ()
  "Generate autoload lines for all defined muggles.
Also generates global keybindings if any."
  (let ((muggles (emacspeak-muggles-enumerate))
        (buff
         (find-file-noselect
          (expand-file-name "emacspeak-muggles-autoloads.el"
                            emacspeak-lisp-directory))))
    (with-current-buffer buff
      (erase-buffer)
      (insert ";;; Auto Generated: Do Not Hand Edit.\n\n")
      (cl-loop
       for m in muggles do
       (let ((key  (where-is-internal m nil 'first)))
         (insert (format "(autoload '%s \"emacspeak-muggles\" \"%s\" t)\n" m m))
         (when key 
           (insert (format "(global-set-key %s '%s)\n" key m)))))
      (insert "\n(provide 'emacspeak-muggles-autoloads)\n")
      (save-buffer))
    (message "Generated autoloads for muggles.")))

;;}}}
;;{{{ smartParens:

(global-set-key
 (ems-kbd "C-c ,")
 (defhydra emacspeak-muggles-smartparens
   (:body-pre
    (progn
      (when hydra-is-helpful (emacspeak-hydra-toggle-talkative))
      (emacspeak-hydra-body-pre "SmartParens"))
    :pre emacspeak-hydra-pre
    :post emacspeak-hydra-post)
   "Smart Parens"
   ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))  
   ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))  
   ("<down>" sp-splice-sexp-killing-forward)  
   ("<left>" sp-forward-barf-sexp)  
   ("<right>" sp-forward-slurp-sexp)  
   ("<up>" sp-splice-sexp-killing-backward)  
   ("?" (emacspeak-hydra-self-help "emacspeak-muggles-smartparens"))
   ("C-<left>" sp-backward-barf-sexp)  
   ("C-<right>" sp-backward-slurp-sexp)
   ("R" sp-splice-sexp)  
   ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))  
   ("a" beginning-of-defun)
   ("b" sp-backward-sexp)  
   ("c" sp-convolute-sexp)  
   ("d" sp-down-sexp)  
   ("e" end-of-defun)
   ("f" sp-forward-sexp)  
   ("i" sp-indent-defun)  
   ("j" sp-join-sexp)  
   ("k" sp-kill-sexp)  
   ("n" sp-next-sexp)  
   ("p" sp-previous-sexp)  
   ("r" sp-splice-sexp-killing-around)  
   ("s" sp-split-sexp)  
   ("t" sp-transpose-sexp)  
   ("u" sp-backward-up-sexp)  
   ("w" sp-copy-sexp)  
   ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))))

;;}}}
;;{{{  View Mode:

(global-set-key
 (ems-kbd
  "C-. v")
 (defhydra emacspeak-muggles-view
   (:body-pre
    (progn
      (emacspeak-hydra-toggle-talkative)
      (emacspeak-hydra-body-pre "View"))
    :hint nil
    :pre emacspeak-hydra-pre :post emacspeak-hydra-post)
   "View Mode"
   ("?" (emacspeak-hydra-self-help "emacspeak-muggles-view"))
   ("$" set-selective-display)
   ("%"  View-goto-percent)
   ("'" register-to-point)
   ("(" backward-sexp)
   (")" forward-sexp)
   ("." set-mark-command)
   ("/" View-search-regexp-forward)
   ("<" beginning-of-buffer)
   ("<return>" nil "quit")
   ("=" what-line)
   (">" end-of-buffer)
   ("@" View-back-to-mark)
   ("A"beginning-of-defun)
   ("DEL" View-scroll-page-backward)
   ("E"end-of-defun)
   ("J" (emacspeak-hide-or-expose-block 'all))
   ("SPC" View-scroll-page-forward)
   ("[" backward-page)
   ("\\" View-search-regexp-backward)
   ("]" forward-page)
   ("a" move-beginning-of-line)
   ("b" backward-word)
   ("c" emacspeak-speak-char)
   ("d" View-scroll-half-page-forward)
   ("e" move-end-of-line)
   ("f" forward-word)
   ("g" goto-line)
   ("h" backward-char)
   ("i" emacspeak-speak-mode-line)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("m" point-to-register)
   ("n" View-search-last-regexp-forward)
   ("p" View-search-last-regexp-backward)
   ("q" nil "quit")
   ("r" copy-to-register)
   ("s" emacspeak-hydra-toggle-talkative)
   ("t" (recenter 0))
   ("u" View-scroll-half-page-backward)
   ("w"emacspeak-speak-word)
   ("x" exchange-point-and-mark)
   ("y" kill-ring-save)
   ("{" backward-paragraph)
   ("}" forward-paragraph)
   ))

;;}}}
;;{{{Vuiet:

(declare-function emacspeak-vuiet-track-info "emacspeak-vuiet" nil)
(when (locate-library "vuiet")
  (with-no-warnings
    (global-set-key
     (ems-kbd "C-; v")
     (defhydra emacspeak-muggles-vuiet
       (:body-pre
        (progn
          (when hydra-is-helpful (emacspeak-hydra-toggle-talkative))
          (emacspeak-hydra-body-pre "Vuiet  Explorer"))
        :pre emacspeak-hydra-pre :post emacspeak-hydra-post)
       (";" vuiet-playing-track-lyrics)
       ("=" vuiet-player-volume-inc)
       ("-" vuiet-player-volume-dec)
       ("A" vuiet-play-artist-loved-tracks)
       ("'" vuiet-play-loved-tracks)
       ("," vuiet-seek-backward)
       ("." vuiet-seek-forward)
       ("C-s" vuiet-artist-info-search)
       ("L" vuiet-playing-artist-lastfm-page)
       ("SPC" vuiet-play-pause)
       ("a" vuiet-artist-info)
       ("i" emacspeak-vuiet-track-info)
       ("l" vuiet-love-track)
       ("n" vuiet-next)
       ("p" vuiet-play-artist)
       ("r" vuiet-replay)
       ("s" vuiet-stop)
       ("t" vuiet-play-track)
       ("u" vuiet-unlove-track)))))

;;}}}
(provide 'extra-muggles)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-warnings: (not noruntime )
;;; end:

;;}}}
