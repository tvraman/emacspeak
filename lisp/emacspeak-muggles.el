;;; emacspeak-muggles.el --- Convenience Hydras For The Emacspeak Desktop
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable MUGGLES An Emacs Interface to muggles
;;; Keywords: Emacspeak,  Audio Desktop muggles
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
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
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; MUGGLES ==  Keybinding Conveniences For The Emacspeak Desktop.

;;; This module implements no new functionality --- contrast with
;;; emacspeak-wizards.  Instead, it uses package hydra to provide
;;; convenience key-bindings that access existing Emacspeak
;;; functionality.
;;; This module is automatically loaded if package Hydra is loaded.
;;; You need to install package Hydra first:
;;; @samp{M-x package-install  hydra}.

;;; Note that on newer versions of Emacs, loading this module will
;;; attempt to automatically install package hydra if it is not found.

;;; @subsection Implemented Muggles

;;;@itemize
;;; @item Brightness: <print> Control display brightness using xbacklight.
;;; @item View-Mode: <C-c v> Temporarily behave like view-mode.
;;; @item Navigate: <s-n> Navigate with ease.
;;;@item  org-mode structure nav: <C-c SPC> Structure navigation  for org-mode.
;;;@item  org-mode tables: <C-c t> Table UI for org-mode tables.
;;;@item m-player: <s-m> Emacspeak-M-Player Commands
;;; @item hideshow: C-c h Provide HideShow bindings.
;;; @item toggle-option:  <C-c o> Single binding for toggling options.
;;; @item outliner: <C-c .> Bindings from outline-minor-mode.
;;;@item Info-Summary: <?> in Info Info Summary Muggle
;;;@end itemize

;;; Emacspeak automatically speaks Hydra hints when displayed.
;;; To silence all Hydra hints, set hydra-is-helpful to nil.  To
;;; temporarily silence speaking of Hydra hints, Muggles can bind
;;; command @code{emacspeak-muggles-toggle-talkative}.  As an
;;; example, Muggle @samp{ViewMode} binds @code{s} to this command.

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl)
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(cl-eval-when '(load)
(when (locate-library "package")
  (unless (locate-library "hydra") (package-install 'hydra))))
(require 'hydra "hydra" 'no-error)(require 'xbacklight)
(require 'view)
(require 'org)
(eval-when-compile(require 'emacspeak-m-player))

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
 (kbd "s-m")
 (emacspeak-muggles-generate 'emacspeak-m-player-mode-map))

;;}}}
;;{{{ Map Hydra Colors To Voices:

(voice-setup-add-map
 '(
   (hydra-face-red voice-bolden)
   (hydra-face-blue voice-lighten)
   (hydra-face-amaranth voice-animate)
   (hydra-face-pink voice-bolden-medium)
   (hydra-face-teal voice-lighten-medium)))

;;}}}
;;{{{ Toggle Talkative:

(defvar emacspeak-muggles-talkative-p t
  "Set to nil when silencing speaking of hydra hints.")

(defun emacspeak-muggles-toggle-talkative ()
  "Toggle state of emacspeak-muggles-talkative-p."
  (interactive)
  (declare (special emacspeak-muggles-talkative-p))
  (setq emacspeak-muggles-talkative-p (not emacspeak-muggles-talkative-p))
  (emacspeak-auditory-icon (if emacspeak-muggles-talkative-p 'on 'off)))

;;}}}
;;{{{ Emacspeak Helpers:

(defun emacspeak-muggles-body-pre (&optional name)
  "Provide auditory icon"
  (when name (dtk-speak name))
  (emacspeak-auditory-icon 'open-object))

(defun emacspeak-muggles-pre ()
  "Provide auditory icon"
  (emacspeak-auditory-icon 'progress))

(defun emacspeak-muggles-post ()
  "Provide auditory icon.
Also turn on emacspeak-muggles-talkative-p if it was turned off."
  (setq emacspeak-muggles-talkative-p t)
  (emacspeak-auditory-icon 'close-object))

;;}}}
;;{{{ Advice LV:

(setq hydra-head-format "%s ")

(defadvice lv-message (after emacspeak pre act comp)
  "provide spoken feedback if idle, and emacspeak-muggles-talkative-p is T."
  (when emacspeak-muggles-talkative-p
    (let ((buffer (get-buffer "*LV*"))
          (dtk-stop-immediately  nil))
      (when (and buffer  (buffer-live-p buffer))
        (with-current-buffer buffer
          (dtk-speak-list
           (split-string
            (propertize
             (buffer-substring (point-min) (1- (point-max)))
             :personality 'voice-smoothen)
            ",")))))))

;;}}}
;;{{{ Brightness:

(global-set-key
 (kbd "<print>")
 (defhydra emacspeak-muggles-brightness
   (:body-pre (emacspeak-muggles-body-pre "Brightness")
              :pre emacspeak-muggles-pre
              :post emacspeak-muggles-post)
   "Brightness"
   ("s" xbacklight-set "set")
   ("g" xbacklight-get "Get")
   ("<print>" xbacklight-black "black")
   ("0" xbacklight-black "black")
   ("1" xbacklight-white  "white")
   ("d" xbacklight-decrement "dimmer")
   ("i" xbacklight-increment "brighter")
   ("SPC" xbacklight-increment "brighter")))

;;}}}
;;{{{  View Mode:

(global-set-key
 (kbd  "C-c v")
 (defhydra emacspeak-muggles-view
   (:body-pre
    (progn
      (emacspeak-muggles-toggle-talkative)
      (emacspeak-muggles-body-pre "View"))
    :hint nil
    :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
   "View Mode"
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
   ("[" previous-page)
   ("\\" View-search-regexp-backward)
   ("]" next-page)
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
   ("s" emacspeak-muggles-toggle-talkative)
   ("t" (recenter 0))
   ("u" View-scroll-half-page-backward)
   ("w"emacspeak-speak-word)
   ("x" exchange-point-and-mark)
   ("y" kill-ring-save)
   ("{" backward-paragraph)
   ("}" forward-paragraph)
   ))

;;}}}
;;{{{ Org Mode Structure Navigation:

(define-key org-mode-map 
  (kbd "C-c SPC")
  (defhydra emacspeak-muggles-org-nav
    (:body-pre
     (progn
       (emacspeak-muggles-toggle-talkative)
       (emacspeak-muggles-body-pre "OrgNavView"))
     :hint nil
     :pre emacspeak-muggles-pre :post emacspeak-muggles-post
     :color red :columns 3)
    "Org Mode Movements"
    ("SPC" emacspeak-outline-speak-this-heading  "Speak this section")
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t)))

;;}}}
;;{{{ Org-Mode Table Navigation:

(define-key
  org-mode-map (kbd "C-c t")
  (defhydra emacspeak-muggles-org-table
    (:body-pre (emacspeak-muggles-body-pre "Org Table UI")
               :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
    "Org Table UI"
    ("j" org-table-next-row)
    ("k" org-table-previous-row)
    ("h" org-table-previous-field)
    ("l" org-table-next-field)
    ("SPC"emacspeak-org-table-speak-current-element)
    ("."emacspeak-org-table-speak-coordinates)
    ("b"emacspeak-org-table-speak-both-headers-and-element)
    ("r"emacspeak-org-table-speak-row-header-and-element)
    ("c"emacspeak-org-table-speak-column-header-and-element)))

;;}}}
;;{{{ Media Player:

(defhydra emacspeak-muggles-m-player
  (:body-pre (emacspeak-muggles-body-pre "Media Player")
             :hint nil
             :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
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
  ("L" emacspeak-m-player-load-file)
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
  ("m" emacspeak-m-player-speak-mode-line)
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
  )

;;}}}
;;{{{ HideShow:

(global-set-key
 (kbd "C-c h")
 (defhydra  emacspeak-muggles-hideshow
   (
    :body-pre (emacspeak-muggles-body-pre  "Hide Show")
              :pre emacspeak-muggles-pre :post emacspeak-muggles-post :color blue)
   "Hideshow"
   ("h" hs-hide-block)
   ("s" hs-show-block)
   ("H" hs-hide-all)
   ("S" hs-show-all)
   ("i" hs-hide-initial-comment-block)))

;;}}}
;;{{{ Option Toggle

;;; Cloned from hydra-examples.el and modified to tase.
(require 'whitespace)
(global-set-key
 (kbd "C-c o")
 (defhydra emacspeak-muggles-toggle-option
   (:color blue :body-pre (emacspeak-muggles-body-pre "Toggle Option ")
           :pre emacspeak-muggles-pre :post emacspeak-muggles-post
           )
   "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_g_ debug-on-quit:    %`debug-on-quit
_h_ hydra-is-helpful    %`hydra-is-helpful
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode

"
   ("a" abbrev-mode)
   ("d" toggle-debug-on-error)
   ("f" auto-fill-mode)
   ("g"  toggle-debug-on-quit)
   ("h" (setq hydra-is-helpful (not hydra-is-helpful)))
   ("t" toggle-truncate-lines)
   ("w" whitespace-mode)
   ("q" nil "quit")))

;;}}}
;;{{{ Outliner:

;;; Cloned from Hydra Wiki:
(global-set-key
 (kbd "C-c .")
 (defhydra emacspeak-muggles-outliner
   (:body-pre (progn
                (outline-minor-mode 1)
                (emacspeak-muggles-body-pre "Outline Navigation"))
              :pre emacspeak-muggles-pre :post emacspeak-muggles-post
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
   ;; Hide
   ("q" hide-sublevels)    ; Hide everything but the top-level headings
   ("t" hide-body)         ; Hide everything but headings (all body lines)
   ("o" hide-other)        ; Hide other branches
   ("c" hide-entry)        ; Hide this entry's body
   ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
   ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
   ;; Show
   ("a" show-all)          ; Show (expand) everything
   ("e" show-entry)        ; Show this heading's body
   ("i" show-children)     ; Show this heading's immediate child sub-headings
   ("k" show-branches)     ; Show all sub-headings under this heading
   ("s" show-subtree)      ; Show (expand) everything in this heading & below
   ;; Move
   ("u" outline-up-heading)                ; Up
   ("n" outline-next-visible-heading)      ; Next
   ("p" outline-previous-visible-heading)  ; Previous
   ("f" outline-forward-same-level)        ; Forward - same level
   ("b" outline-backward-same-level)       ; Backward - same level
   ("z" nil "leave")))

;;}}}
;;{{{ Navigate:

;;; Inspired by  Hydra wiki:
;;; But bound to s-n --- instead of C-n

(global-set-key
 (kbd "s-n")
 (defhydra emacspeak-muggles-navigate
   (:body-pre
    (progn
      (emacspeak-muggles-body-pre "Move")
      (emacspeak-muggles-toggle-talkative)
      (condition-case nil (next-line) (error nil)))
    :hint nil
    :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
   "move"
   ("s" emacspeak-muggles-toggle-talkative)
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("j" next-line)
   ("k" previous-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)))

;;}}}
;;{{{ Info Summary:

;;; Taken from Hydra wiki and customized to taste:
(define-key Info-mode-map (kbd "?")
  (defhydra emacspeak-muggles-info-summary
    (:color blue :hint nil
            :body-pre (emacspeak-muggles-body-pre "Info Summary")
            :pre emacspeak-muggles-pre :post emacspeak-muggles-post)
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
    ("q"   Info-exit "Info exit")
    ("C-g" nil "cancel" :color blue)))

;;}}}
(provide 'emacspeak-muggles)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
