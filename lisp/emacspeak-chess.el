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
;;;
;;; The Emacs Chess package provides a rich environment for playing and
;;; exploring Chess Games.
;;; That package comes with a light-weight module that announces
;;; moves.
;;;
;;; This module aims do do much more, including:
;;; @itemize @bullet
;;; @item Navigate the board along various axies with audio-formatted  output.
;;;  @item Browse games via  rich audio-formatted   output.
;;; @item Speech-enable all interactive commands  provided by the Chess
;;; package.
;;; @item Enable various means of exploring the state of game, perhaps with
;;; a view to being able to spot patterns   from listening to the
;;; output.
;;; @end itemize 
;;; @subsection Board Navigation:
;;; The board can be navigated along the 8 compass directions.
;;; Arrow keys move to the appropriate squares on the board.
;;; @kbd{/} and @kbd{\} move down the diagonals.
;;; @kbd{[} and @kbd{]} move up the respective diagonals.
;;; @itemize  @bullet
;;; @item  Move North: @code{emacspeak-chess-north} bound to @kbd{up}.
;;; @item  Move South: @code{emacspeak-chess-south} bound to
;;; @kbd{down}.
;;; @item  Move West: @code{emacspeak-chess-west} bound to @kbd{left}.
;;; @item  Move East: @code{emacspeak-chess-east} bound to @kbd{right}.
;;; @end itemize
;;; You can also move along the diagonals:
;;; @itemize @bullet
;;; @item  Move Northeast: @code{emacspeak-chess-northeast} bound to  @kbd{]}.
;;; @item  Move Northwest: @code{emacspeak-chess-northwest} bound to
;;; @kbd{[}.
;;; @item  Move Southwest: @code{emacspeak-chess-southwest} bound to
;;; @kbd{/}.
;;; @item  Move Southeast: @code{emacspeak-chess-southeast} bound to
;;; @kbd{\}.
;;; @end itemize
;;; You can also jump to a given board position by:
;;; @itemize @bullet
;;; @item  Jump: @code{emacspeak-chess-jump} bound to @kbd{j}.
;;; @item Look: @code{emacspeak-chess-speak-that-square} bound to
;;;@kbd{l}.
;;; @item  Review   current square: @kbd{;}.
;;; @end itemize
;;; You can obtain views of the board along the rows and diagonals:
;;; @itemize @bullet
;;; @item Viewers: @kbd{v} followed by the directional navigation keys
;;;speaks the squares in that direction from the current square.
;;; @end itemize 

;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(eval-when-compile
  (require 'chess-pos nil 'noerror)
  (require 'chess-display nil 'noerror))

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
    (?\  . ""))
  "Piece-char to piece-name mapping.")

(defsubst emacspeak-chess-piece-name (char)
  "Return piece name."
  (cdr (assq (downcase char) emacspeak-chess-piece-names)))

(defun emacspeak-chess-describe-square (index)
  "Return an audio formatted description of square at given index
  as a list.  Argument index is an integer between 0 and 63 as in
  package chess."
  (cl-declare (special chess-module-game chess-display-index))
  (cl-assert (eq major-mode 'chess-display-mode) t "Not in a Chess  display.")
  (let ((position (chess-game-pos chess-module-game chess-display-index))
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
    (unless white
      (setq piece (propertize  piece 'personality voice-bolden)))
    (if light ;;; square color
        (setq coord (propertize  coord 'personality voice-monotone))
      (setq coord (propertize  coord 'personality voice-lighten-extra )))
    (if (zerop (length piece)) 
        (list coord  )
      (list coord  piece))))


(defun emacspeak-chess-speak-this-square ()
  "Speak square under point."
  (interactive)
  (cl-assert (eq major-mode 'chess-display-mode) t "Not in a Chess  display.")
  (let ((index (get-text-property (point) 'chess-coord)))
    (cl-assert index t "Not on a valid square.")
    (dtk-speak-list  (emacspeak-chess-describe-square index))))


(defun emacspeak-chess-speak-that-square (coord)
  "Speak square at specified coord."
  (interactive "sCoord: ")
  (cl-assert (eq major-mode 'chess-display-mode) t "Not in a Chess  display.")
  (let ((index (chess-coord-to-index coord)))
    (cl-assert index t "Not  a valid square.")
    (dtk-speak-list  (emacspeak-chess-describe-square index) 2)))
;;}}}
;;{{{Board Navigation:

(defun emacspeak-chess-jump (coord)
  "Jump to square specified as coord."
  (interactive "sCoord: ")
  (goto-char
   (chess-display-index-pos
    (current-buffer) (chess-coord-to-index coord)))
  (emacspeak-auditory-icon 'large-movement)
  (emacspeak-chess-speak-this-square))

(defun emacspeak-chess-move (direction)
  "Move in direction by one step."
  (let ((index (get-text-property (point) 'chess-coord))
        (target nil))
    (cl-assert index t "Not on a valid square.")
    (setq target (chess-next-index  index direction))
    (unless target (error "Edge of board"))
    (goto-char (chess-display-index-pos (current-buffer) target))
    (emacspeak-auditory-icon 'item)
    (emacspeak-chess-speak-this-square)))

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
;;{{{Examining the board:

(defun emacspeak-chess-collect-squares (direction)
  "Collect descriptions of squares along given direction from current position."
  (let ((index (get-text-property (point) 'chess-coord))
        (target nil)
        (result nil)
        (squares nil))
    (cl-assert index t "Not on a valid square.")
    (push (emacspeak-chess-describe-square index) squares)
    (setq target (chess-next-index  index direction))
    (while target 
      (push (emacspeak-chess-describe-square target) squares)
      (setq target (chess-next-index  target direction)))
    (setq result (nreverse squares))
    (flatten-list
     (cl-loop
      for s in result
      when
      (= (length s) 2)
      collect s))))

(defun emacspeak-chess-look-north ()
  "Look north "
  (interactive)
  (cl-declare (special chess-direction-north))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-north)
   2))

(defun emacspeak-chess-look-south ()
  "Look south "
  (interactive)
  (cl-declare (special chess-direction-south))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-south)
   2))

(defun emacspeak-chess-look-west ()
  "Look west "
  (interactive)
  (cl-declare (special chess-direction-west))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-west)
   2))

(defun emacspeak-chess-look-east ()
  "Look east "
  (interactive)
  (cl-declare (special chess-direction-east))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-east)
   2))

(defun emacspeak-chess-look-northwest ()
  "Look northwest "
  (interactive)
  (cl-declare (special chess-direction-northwest))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-northwest)
   2))

(defun emacspeak-chess-look-southwest ()
  "Look southwest "
  (interactive)
  (cl-declare (special chess-direction-southwest))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-southwest)
   2))

(defun emacspeak-chess-look-northeast ()
  "Look northeast "
  (interactive)
  (cl-declare (special chess-direction-northeast))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-northeast)
   2))

(defun emacspeak-chess-look-southeast ()
  "Look southeast "
  (interactive)
  (cl-declare (special chess-direction-southeast))
  (emacspeak-auditory-icon 'task-done) 
  (dtk-speak-list
   (emacspeak-chess-collect-squares chess-direction-southeast)
   2))


(define-prefix-command 'emacspeak-chess-view-prefix
  'emacspeak-chess-view-map)

(cl-declaim (special emacspeak-chess-view-map))
(cl-loop
 for binding in
 '(
   ("<up>" emacspeak-chess-look-north)
   ("<down>" emacspeak-chess-look-south)
   ("<left>" emacspeak-chess-look-west)
   ("<right>" emacspeak-chess-look-east)
   ("[" emacspeak-chess-look-northwest)
   ("]" emacspeak-chess-look-northeast)
   ("\\" emacspeak-chess-look-southeast)
   ("/" emacspeak-chess-look-southwest)
   )
 do
 (emacspeak-keymap-update emacspeak-chess-view-map binding))

;;}}}
;;{{{Speaking Moves:

(defun emacspeak-chess-describe-move (game &optional move-index)
  "Speak the move by examining game.  Optional argument `move-index'
specifies index of move default is final index."
  (let*
      ((ply 
        (chess-game-ply
         game ;;; ply before specified game-index
         (1- (or move-index (chess-game-index game)))))
       (pos (chess-ply-pos ply))
       (text nil)
       (source (chess-ply-source ply))
       (target (chess-ply-target ply))
       (s-piece (and source (chess-pos-piece pos source)))
       (t-piece (and target (chess-pos-piece pos target)))
       (which (chess-ply-keyword ply :which))
       (promotion (chess-ply-keyword ply :promote)))
    (if which (setq which (char-to-string which)))
    (cond
     ((chess-ply-keyword ply :castle)
      (setq text "short castle"))
     ((chess-ply-keyword ply :long-castle)
      (setq text "long castle"))
     ((and s-piece t-piece (= t-piece ?\ ) target) ;;; target: empty square
      (setq text
            (concat which
                    (format 
                     "%s to %s"
                     (emacspeak-chess-piece-name s-piece)
                     (chess-index-to-coord target)))))
     ((and s-piece t-piece target)
      (setq text
            (concat which
                    (format 
                     "%s takes %s at %s"
                     (emacspeak-chess-piece-name s-piece)
                     (emacspeak-chess-piece-name t-piece)
                     (chess-index-to-coord target))))))
;;; additional consequences of move:
    (if promotion
        (setq text
              (concat 
               text ", "
               (format 
                "promotes  to %s"
                (emacspeak-chess-piece-name promotion)))))
    (if (chess-ply-keyword ply :en-passant)
        (setq text (concat text ", " "en passant")))
    (if (chess-ply-keyword ply :check)
        (setq text (concat text ", " "check")))
    (if (chess-ply-keyword ply :checkmate)
        (setq text (concat text ", " "checkmate ")))
    (if (chess-ply-keyword ply :stalemate)
        (setq text (concat text ", " "stalemate ")))
    text))

;;}}}
;;{{{ Interactive Commands:

'(
  
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
  
  chess-display-mouse-select-piece
  chess-display-mouse-set-piece
  
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

(defadvice chess-display-move-first (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (ems-with-messages-silenced
        (emacspeak-auditory-icon 'left)
        (message "At start of game"))))

(defadvice chess-display-move-last (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (ems-interactive-p)
    (ems-with-messages-silenced
        (emacspeak-auditory-icon 'right)
      (dtk-speak  (emacspeak-chess-describe-move chess-module-game)))))


(defadvice chess-display-move-backward (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cl-declare (special chess-display-index chess-module-game))
  (when (ems-interactive-p)
    (ems-with-messages-silenced
        (emacspeak-auditory-icon 'left)
      (let ((msg nil))
        (setq
         msg
         (cond
          ((= chess-display-index (chess-game-index chess-module-game))
           "Current state  ")
          ((= 0 chess-display-index)
           "Start of game ")))
        (dtk-speak-and-echo
         (concat msg
                 (emacspeak-chess-describe-move chess-module-game chess-display-index)))))))

(defadvice chess-display-move-forward (after emacspeak pre act comp)
  "Provide auditory feedback."
  (cl-declare (special chess-display-index chess-module-game))
  (when (ems-interactive-p)
    (ems-with-messages-silenced
        (emacspeak-auditory-icon 'right)
      (let ((msg nil))
        (setq
         msg
         (cond
          ((= chess-display-index (chess-game-index chess-module-game))
           "Current state  ")
          ((= 0 chess-display-index)
           "Start of game ")))
        (dtk-speak-and-echo
         (concat msg
                 (emacspeak-chess-describe-move chess-module-game chess-display-index)))))))

;;}}}
;;{{{emacspeak Handler:

(defun chess-emacspeak-handler (game event &rest args)
  "Speak the move."
  (cond
   ((eq event 'initialize)
    (emacspeak-chess-setup)
    (emacspeak-auditory-icon 'open-object)
    (message "Buffer %s" (current-buffer))
    t)
   ((eq event 'move)
    (emacspeak-auditory-icon 'time)
    (dtk-speak  (emacspeak-chess-describe-move game)))
   ((eq event 'kibitz)
    (message (car args)))))

(provide 'chess-emacspeak)
;;}}}
;;{{{Emacspeak Setup:
;;; Forward Declaration to help documentation builder.
(defvar chess-default-modules nil)

(defun emacspeak-chess-setup ()
  "Emacspeak setup for Chess."
  (cl-declare (special chess-default-modules
                       chess-display-mode-map))
  (when (bound-and-true-p chess-default-modules)
    (setq chess-default-modules
          (cl-remove
           '(chess-sound chess-announce)
           chess-default-modules :test 'equal))
    (cl-pushnew 'chess-emacspeak chess-default-modules))
  (when (and (bound-and-true-p chess-display-mode-map)
             (keymapp chess-display-mode-map))
    (cl-loop
     for binding in 
     '(
       ( ";" emacspeak-chess-speak-this-square)
       ("v" emacspeak-chess-view-prefix)
       ("<up>" emacspeak-chess-north)
       ("<down>" emacspeak-chess-south)
       ("<left>" emacspeak-chess-west)
       ("<right>" emacspeak-chess-east)
       ("[" emacspeak-chess-northwest)
       ("]" emacspeak-chess-northeast)
       ("\\" emacspeak-chess-southeast)
       ("/" emacspeak-chess-southwest)
       ("l" emacspeak-chess-speak-that-square)
       ("j" emacspeak-chess-jump))
     do
     (emacspeak-keymap-update chess-display-mode-map binding))))

;;}}}
(provide 'emacspeak-chess)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
