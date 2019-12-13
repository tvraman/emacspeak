;;; emacspeak-chess.el --- Speech-enable CHESS  -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable CHESS An Emacs Interface to chess
;;; Keywords: Emacspeak,  Audio Desktop chess
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;;; MERCHANTABILITY or FITNCHESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:
;;; CHESS ==  The Game Of Chess
;;; The Emacs Chess package enables a rich environment for playing and
;;; exploring Chess Games.
;;; That package comes with a light-weight module that announces
;;; moves.

;;; This module aims do do much more, including:
;;; Browse games via  rich audio-formatted   output.
;;; Speech-enable all interactive commands  provided by the Chess
;;; package
;;; Enable various means of exploring the state of game, perhaps with
;;; a view to being able to spot patterns   from listening to the
;;; output.
;;;
;;; @subsection Board Navigation:
;;; Arrow keys move to the appropriate cells on the board.  
;;; @itemize  @bullet
;;; @item  Move North: @code{emacspeak-chess-north} bound to @kbd{up}.
;;; @item  Move South: @code{emacspeak-chess-south} bound to
;;; @kbd{down}.
;;; @item  Move West: @code{emacspeak-chess-west} bound to @kbd{left}.
;;; @item  Move East: @code{emacspeak-chess-east} bound to @kbd{right}.
;;; @end itemize
;;; You can also move along the diagonals:
;;; @itemize @bullet
;;; @item  Move Northeast: @code{emacspeak-chess-northeast} bound to  @kbd{[}.
;;; @item  Move Northwest: @code{emacspeak-chess-northwest} bound to
;;; @kbd{]}.
;;; @item  Move Southwest: @code{emacspeak-chess-southwest} bound to
;;; @kbd{/}.
;;; @item  Move Southeast: @code{emacspeak-chess-southeast} bound to
;;; @kbd{\\}.
;;; @end itemize
;;; You can also jump to a given board position by:
;;; @itemize @bullet
;;; @item  Jump: @code{emacspeak-chess-jump} bound to @kbd{j}.
;;; @end itemize
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile
  (require 'chess-pos)
  (require 'chess-display))

;;}}}
;;{{{ Map Faces:

(voice-setup-add-map
 '(
   (chess-display-black-face voice-bolden)
   (chess-display-white-face voice-lighten)
   (chess-ics1-black-face voice-bolden)
   (chess-ics1-highlight-face voice-animate)
   (chess-ics1-white-face voice-lighten)
   (chess-plain-black-face voice-bolden)
   (chess-plain-highlight-face voice-animate)
   (chess-plain-white-face voice-lighten)))

;;}}}
;;{{{Helpers:

(defvar emacspeak-chess-piece-names
  '((?q . "queen")
    (?k . "king")
    (?b . "bishop")
    (?n . "knight")
    (?r . "rook")
    (?p . "pawn")
    (?\  . "empty"))
  "Piece-char to piece-name mapping.")

(defsubst emacspeak-chess-piece-name (char)
  "Return piece name."
  (cdr (assq (downcase char) emacspeak-chess-piece-names)))

(defun emacspeak-chess-describe-cell (index)
  "Return an audio formatted description of cell at given index.
  Argument index is an integer between 0 and 63 as in package chess."
  (cl-assert (eq major-mode 'chess-display-mode) t "Not in a Chess  display.")
  (let ((position (chess-display-position nil))
        (piece nil)
        (white nil)
        (light nil)
        (rank nil)
        (file nil)
        (coord nil))
    (cl-assert position t "Could not retrieve game position.")
    (setq coord (chess-index-to-coord index)
          piece (chess-pos-piece position index)
          rank (chess-index-rank index)
          file (chess-index-file index)
          light                         ; square color
          (or
           (and (cl-evenp rank ) (cl-evenp file))
           (and (cl-oddp rank) (cl-oddp file)))
          white (memq piece '(?R ?N ?B ?K ?Q ?P)) ;upper-case is white
          piece (emacspeak-chess-piece-name  piece))
    (unless white (setq piece (propertize  piece 'personality voice-bolden)))
    (unless light (setq coord (propertize  coord 'personality voice-bolden)))
    (concat piece " on " coord)))

(defun emacspeak-chess-speak-this-cell ()
  "Speak cell under point."
  (interactive)
  (cl-assert (eq major-mode 'chess-display-mode) t "Not in a Chess  display.")

  (let ((index (get-text-property (point) 'chess-coord)))
    (cl-assert index t "Not in a valid cell.")
    (message (emacspeak-chess-describe-cell index))))

;;}}}
;;{{{Emacspeak Chess Keymap

(cl-declaim (special chess-display-mode-map))
(define-key chess-display-mode-map ";" 'emacspeak-chess-speak-this-cell)
(define-key chess-display-mode-map (kbd "<up>") 'emacspeak-chess-north)
(define-key chess-display-mode-map (kbd "<down>") 'emacspeak-chess-south)
(define-key chess-display-mode-map (kbd "<left>") 'emacspeak-chess-west)
(define-key chess-display-mode-map (kbd "<right>") 'emacspeak-chess-east)
(define-key chess-display-mode-map (kbd "[") 'emacspeak-chess-northwest)
(define-key chess-display-mode-map (kbd "]") 'emacspeak-chess-northeast)
(define-key chess-display-mode-map (kbd "\\") 'emacspeak-chess-southeast)
(define-key chess-display-mode-map (kbd "/") 'emacspeak-chess-southwest)
(define-key chess-display-mode-map (kbd "j") 'emacspeak-chess-jump)



;;}}}
;;{{{Buffer Navigation:

(defun emacspeak-chess-jump (coord)
  "Jump to cell specified as coord."
  (interactive "sCoord: ")
  (goto-char
   (chess-display-index-pos
    (current-buffer) (chess-coord-to-index coord)))
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-chess-speak-this-cell))

(defun emacspeak-chess-move (direction)
  "Move in direction by one step."
  (let ((index (get-text-property (point) 'chess-coord))
        (target nil))
    (cl-assert index t "Not on a valid cell.")
    (setq target (chess-next-index  index direction))
    (unless target (error "Edge of board"))
    (goto-char (chess-display-index-pos (current-buffer) target))
    (emacspeak-auditory-icon 'item)
    (emacspeak-chess-speak-this-cell)))

(defun emacspeak-chess-north ()
  "Move north one step."
  (interactive)
  (cl-declare (special chess-direction-north))
  (emacspeak-chess-move chess-direction-north))

(defun emacspeak-chess-south ()
  "Move south one step."
  (interactive)
  (cl-declare (special chess-direction-south))
  (emacspeak-chess-move chess-direction-south))

(defun emacspeak-chess-west ()
  "Move west one step."
  (interactive)
  (cl-declare (special chess-direction-west))
  (emacspeak-chess-move chess-direction-west))

(defun emacspeak-chess-east ()
  "Move east one step."
  (interactive)
  (cl-declare (special chess-direction-east))
  (emacspeak-chess-move chess-direction-east))

(defun emacspeak-chess-northwest ()
  "Move northwest one step."
  (interactive)
  (cl-declare (special chess-direction-northwest))
  (emacspeak-chess-move chess-direction-northwest))

(defun emacspeak-chess-southwest ()
  "Move southwest one step."
  (interactive)
  (cl-declare (special chess-direction-southwest))
  (emacspeak-chess-move chess-direction-southwest))

(defun emacspeak-chess-northeast ()
  "Move northeast one step."
  (interactive)
  (cl-declare (special chess-direction-northeast))
  (emacspeak-chess-move chess-direction-northeast))

(defun emacspeak-chess-southeast ()
  "Move southeast one step."
  (interactive)
  (cl-declare (special chess-direction-southeast))
  (emacspeak-chess-move chess-direction-southeast))

;;}}}
;;{{{Emacspeak Setup:
(defun emacspeak-chess-setup ()
  "Emacspeak setup for Chess."
  (cl-declare (special chess-default-modules))
  (setq chess-default-modules
        (cl-remove
         '(chess-sound chess-announce)
         chess-default-modules :test 'equal)) (cl-pushnew 'chess-emacspeak chess-default-modules))

(defadvice chess-display-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cl-declare (special chess-default-modules))
  (when (ems-interactive-p) (emacspeak-chess-setup)))

;;}}}
;;{{{emacspeak Handler:

(defun chess-emacspeak-handler (game event &rest args)
  "Speak the move."
  (cond
   ((eq event 'initialize)
    (emacspeak-auditory-icon 'open-object)
     t)
   ((eq event 'move)
    (emacspeak-auditory-icon 'item)
    (let* ((ply (chess-game-ply game (1- (chess-game-index game))))
           (pos (chess-ply-pos ply)))
      (unless
          (let* ((source (chess-ply-source ply))
                 (target (chess-ply-target ply))
                 (s-piece (and source (chess-pos-piece pos source)))
                 (t-piece (and target (chess-pos-piece pos target)))
                 (which (chess-ply-keyword ply :which))
                 text)
            (if which
                (setq which (char-to-string which)))
            (cond
             ((chess-ply-keyword ply :castle)
              (setq text (chess-string 'short-castle)))
             ((chess-ply-keyword ply :long-castle)
              (setq text (chess-string 'long-castle)))
             ((and s-piece t-piece (= t-piece ? ) target)
              (setq text
                    (concat which
                            (chess-string 'piece-moves
                                          (emacspeak-chess-piece-name s-piece)
                                          (chess-index-to-coord target)))))
             ((and s-piece t-piece target)
              (setq text
                    (concat which
                            (chess-string 'piece-takes
                                          (emacspeak-chess-piece-name s-piece)
                                          (emacspeak-chess-piece-name t-piece)
                                          (chess-index-to-coord target))))))

            (let ((promotion (chess-ply-keyword ply :promote)))
              (if promotion
                  (setq text
                        (concat text ", "
                                (chess-string 'promote
                                              (emacspeak-chess-piece-name promotion))))))
            (if (chess-ply-keyword ply :en-passant)
                (setq text (concat text ", " (chess-string 'en-passant))))
            (if (chess-ply-keyword ply :check)
                (setq text (concat text ", " (chess-string 'check))))
            (if (chess-ply-keyword ply :checkmate)
                (setq text (concat text ", " (chess-string 'checkmate))))
            (if (chess-ply-keyword ply :stalemate)
                (setq text (concat text ", " (chess-string 'stalemate))))

            (message text)))))
   ((eq event 'kibitz)
    (message (car args)))))

(provide 'chess-emacspeak)
;;}}}
;;{{{ Interactive Commands:

'(
  chess
  chess-debug-position
  chess-display-abort
  chess-display-accept
  chess-display-annotate
  chess-display-call-flag
  chess-display-chat
  chess-display-check-autosave
  chess-display-clear-board
  chess-display-create
  chess-display-decline
  chess-display-draw
  chess-display-duplicate
  chess-display-edit-board
  chess-display-force
  chess-display-highlight-passed-pawns
  chess-display-invert
  chess-display-kill-board
  chess-display-list-buffers
  chess-display-manual-move
  chess-display-match
  chess-display-mode
  chess-display-mouse-select-piece
  chess-display-mouse-set-piece
  chess-display-move-backward
  chess-display-move-first
  chess-display-move-forward
  chess-display-move-last
  chess-display-move-menu
  chess-display-pass
  chess-display-quit
  chess-display-redraw
  chess-display-remote
  chess-display-resign
  chess-display-restore-board
  chess-display-retract
  chess-display-search
  chess-display-search-again
  chess-display-search-backward
  chess-display-search-delete
  chess-display-search-forward
  chess-display-search-key
  chess-display-select-piece
  chess-display-send-board
  chess-display-set-from-fen
  chess-display-set-piece
  chess-display-shuffle
  chess-display-undo
  chess-display-yank-board
  chess-ics
  chess-images-decrease-size
  chess-images-increase-size
  chess-images-set-directory
  chess-input-shortcut
  chess-input-shortcut-delete
  chess-link
  chess-pgn-complete-move
  chess-pgn-insert-and-show-position
  chess-pgn-mode
  chess-pgn-mouse-show-position
  chess-pgn-read
  chess-pgn-show-position
  chess-plain-customize
  chess-polyglot-book-close
  chess-puzzle
  chess-session
  chess-tutorial
  )

;;}}}
(provide 'emacspeak-chess)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
