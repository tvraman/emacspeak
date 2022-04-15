;; emacspeak-chess.el --- Speech-enable CHESS  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable CHESS An Emacs Interface to chess
;; Keywords: Emacspeak,  Audio Desktop chess
;;{{{  LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;  $Revision: 4532 $ |
;; Location undetermined
;; 

;;}}}
;;{{{  Copyright:
;; Copyright (C) 1995 -- 2007, 2019, T. V. Raman
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
;; MERCHANTABILITY or FITNCHESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,MA 02110-1301, USA.

;;}}}
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;; Commentary:
;; 
;; The Emacs Chess package provides a rich environment for playing and
;; exploring Chess Games.
;; That package comes with a light-weight module that announces
;; moves.
;; 
;; This module aims do do much more, including:
;; @itemize @bullet
;; @item Navigate the board along various axies with audio-formatted  output.
;;  @item Browse games via  rich audio-formatted   output.
;; @item Speech-enable all interactive commands  provided by the Chess
;; package.
;; @item Enable various means of exploring the state of game, perhaps with
;; a view to being able to spot patterns   from listening to the
;; output.
;; @end itemize
;; @subsection Navigating And Examining The Board
;; The board can be navigated along the 8 compass directions.
;; Arrow keys move to the appropriate squares on the board.
;; @kbd{/} and @kbd{\} move down the diagonals.
;; @kbd{[} and @kbd{]} move up the respective diagonals.
;; @itemize  @bullet
;; @item  Move North: @code{emacspeak-chess-north} bound to @kbd{up}.
;; @item  Move South: @code{emacspeak-chess-south} bound to
;; @kbd{down}.
;; @item  Move West: @code{emacspeak-chess-west} bound to @kbd{left}.
;; @item  Move East: @code{emacspeak-chess-east} bound to @kbd{right}.
;; @end itemize
;; You can also move along the diagonals:
;; @itemize @bullet
;; @item  Move Northwest: @code{emacspeak-chess-northwest} bound to
;; @kbd{[}.
;; @item  Move Northeast: @code{emacspeak-chess-northeast} bound to  @kbd{]}.
;; @item  Move Southwest: @code{emacspeak-chess-southwest} bound to
;; @kbd{/}.
;; @item  Move Southeast: @code{emacspeak-chess-southeast} bound to
;; @kbd{\}.
;; @end itemize
;; You can also jump to a given board position by:
;; @itemize @bullet
;; @item  Jump: @code{emacspeak-chess-jump} bound to @kbd{j}.
;; @item Target of last move: @code{emacspeak-chess-goto-target} bound to @kbd{t}.
;; @item Look: @code{emacspeak-chess-speak-that-square} bound to
;; @kbd{l}.
;; @item  Review   current square: @kbd{;}.
;; @item Review Board: @code{emacspeak-chess-speak-board} bound to
;; @kbd{z}. Useful in end-state.
;; @item Locate pieces: @code{emacspeak-chess-speak-piece-squares}
;; bound to @kbd{s}. Specify piece as a single char --- @kbd{w} speaks
;; all white pieces, @kbd{l} speaks all black pieces, use SAN notation
;; char for a specific piece.  Use @kbd{a} to hear entire board, this
;; last is most useful when the game is an end-state.
;; @item Discover who targets a square:
;; @code{emacspeak-chess-speak-who-targets} bound to @kbd{w}. Argument
;; @code{piece} is similar to the previously listed command.
;; @end itemize
;; You can obtain views of the board along the rows and diagonals, as
;; well as a @emph{knight's perspective }:
;; @itemize @bullet
;; @item Viewers: @kbd{v} followed by the directional navigation keys
;; speaks the squares in that direction from the current
;; square. @kbd{vn} speaks the squares from a knight's perspective,
;; i.e. the squares the Knight would see  from the current
;; position. similarly, @kbd{vk} speaks the non-empty squares seen by the King
;; from the current position.
;; @kbd{v} followed by a rank-or-file char speaks that complete rank
;; or file.
;; @end itemize
;; @subsection Examining Games

;; Package @emph{Chess} allows one to browse through a game using
;; @kbd{,} and @kbd{.}  --- @code{chess-display-move-backward} and
;; @code{chess-display-move-forward}.  Emacspeak speech-enables these
;; commands to speak the move that led to the currently displayed
;; state of the game. Finally, @kbd{m} speaks the @emph{current move}
;; being displayed.

;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

(require 'chess-pos nil 'noerror)
(require 'chess-display nil 'noerror)

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
(defconst emacspeak-chess-whites
  '(?P ?R ?N ?B ?K ?Q)
  "White chess pieces.")

(defconst emacspeak-chess-blacks
  (mapcar #'downcase emacspeak-chess-whites)
  "Black chess pieces.")

(defun emacspeak-chess-describe-square (index)
  "Return an audio formatted description of square at given index
  as a list.  Argument index is an integer between 0 and 63 as in
  package chess."
  (cl-declare (special chess-module-game chess-display-index
                       emacspeak-chess-whites))
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
          white (memq piece emacspeak-chess-whites) ;upper-case is white
          piece (emacspeak-chess-piece-name  piece))
    (unless white
      (setq piece (propertize  piece 'personality voice-bolden-extra)))
    (if light ;;; square color
        (setq coord (propertize  coord 'personality voice-monotone-extra))
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

(defun emacspeak-chess-goto-target ()
  "Jump to the most recent target square."
  (interactive)
  (cl-declare (special emacspeak-chess-last-target))
  (emacspeak-auditory-icon 'large-movement)
  (cl-assert emacspeak-chess-last-target t "No recent target")
  (goto-char
   (chess-display-index-pos
    (current-buffer)
    emacspeak-chess-last-target))
  (emacspeak-chess-speak-this-square))

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

(defun emacspeak-chess-collect-all-squares ()
  "Collect description of all non-empty squares."
  (let ((result nil)
        (squares nil))
    (cl-loop
     for index  from 0 to 63  do
     (push (emacspeak-chess-describe-square index) squares))
    (setq result (nreverse squares))
    (flatten-list
     (cl-loop
      for s in result
      when (= (length s) 2)
      collect s))))

(defun emacspeak-chess-speak-board ()
  "Speak complete board. Only useful in end-states."
  (interactive)
  (dtk-speak-list (emacspeak-chess-collect-all-squares) 2))

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
   ("v" emacspeak-chess-speak-this-square)
   ("k" emacspeak-chess-look-king)
   ("n" emacspeak-chess-look-knight))
 do
 (emacspeak-keymap-update emacspeak-chess-view-map binding))

(cl-loop
 for key    from 1 to 8 do
 (emacspeak-keymap-update
  emacspeak-chess-view-map
  (list (format "%s" key) 'emacspeak-chess-view-rank-or-file)))

(cl-loop
 for key    from ?a to ?h do
 (emacspeak-keymap-update
  emacspeak-chess-view-map
  (list (format "%c" key) 'emacspeak-chess-view-rank-or-file)))

(defun emacspeak-chess-collect-knight-squares ()
  "List of non-empty squares a knight can reach from current position."
  (let ((index (get-text-property (point) 'chess-coord))
        (kd ;;; knight directions
    (list
     chess-direction-north-northeast chess-direction-east-northeast
     chess-direction-east-southeast chess-direction-south-southeast
     chess-direction-south-southwest chess-direction-west-southwest
     chess-direction-west-northwest chess-direction-north-northwest))
        (result nil)
        (target nil)
        (squares nil))
    (cl-assert index t "Not on a valid square.")
    (cl-loop
     for dir in kd do
     (setq target (chess-next-index  index dir))
     (when target
       (push target squares)))
    (setq result
          (mapcar #'emacspeak-chess-describe-square
                  (nreverse (sort  squares '<))))
    (flatten-list
     (cl-loop
      for s in result
      when
      (= (length s) 2)
      collect s))))

(defun emacspeak-chess-look-knight ()
  "Look along non-empty squares reachable by a knight from current position. "
  (interactive)
  (dtk-speak-list (emacspeak-chess-collect-knight-squares) 2)
  (emacspeak-auditory-icon 'task-done))

(defun emacspeak-chess-collect-king-squares ()
  "List of non-empty squares a king can reach from current position."
  (let ((index (get-text-property (point) 'chess-coord))
        (kd ;;; king directions
    (list
     chess-direction-northwest chess-direction-north chess-direction-northeast
     chess-direction-east
     chess-direction-southeast chess-direction-south chess-direction-southwest
     chess-direction-west))
        (result nil)
        (target nil)
        (squares nil))
    (cl-assert index t "Not on a valid square.")
    (cl-loop
     for dir in kd do
     (setq target (chess-next-index  index dir))
     (when target
       (push target squares)))
    (setq result
          (mapcar #'emacspeak-chess-describe-square
                  (nreverse (sort  squares '<))))
    (flatten-list
     (cl-loop
      for s in result
      when
      (= (length s) 2)
      collect s))))

(defun emacspeak-chess-look-king ()
  "Look along non-empty squares reachable by the king  from current position. "
  (interactive)
  (dtk-speak-list (emacspeak-chess-collect-king-squares) 2)
  (emacspeak-auditory-icon 'task-done))




(defun emacspeak-chess-square-name (index)
  "Return an audio formatted name of square at given index
    Argument index is an integer between 0 and 63 as in
  package chess."
  (cl-assert (eq major-mode 'chess-display-mode) t "Not in a Chess  display.")
  (let ((light nil)
        (rank nil)
        (file nil)
        (coord nil))
    (setq
     coord (chess-index-to-coord index)
     rank (chess-index-rank index)
     file (chess-index-file index)
     light                              ; square color
     (or
      (and (cl-evenp rank ) (cl-evenp file))
      (and (cl-oddp rank) (cl-oddp file))))
    (if light ;;; square color
        (setq coord (propertize  coord 'personality voice-monotone-extra))
      (setq coord (propertize  coord 'personality voice-lighten-extra )))
    coord))

(defun emacspeak-chess-piece-squares (piece)
  "Return a description of where a given piece is on the board."
  (cl-declare (special chess-display-index chess-module-game
                       emacspeak-chess-whites emacspeak-chess-blacks))
  (cl-assert (eq major-mode 'chess-display-mode) t "Not  a Chess display.")
  (cl-assert
   (memq piece
         `(?w ?l ?a
              ,@emacspeak-chess-whites ,@emacspeak-chess-blacks))
   t
   "Specify a piece char, or w for whites and l for blacks")
  (let* ((pos (chess-game-pos chess-module-game chess-display-index))
         (black (= piece ?l))
         (white (= piece ?w))
         (all (= piece ?a))
         (where
          (chess-pos-search
           pos
           (cond
            (black nil)
            (white t)
            (t piece))))
         (color (memq piece emacspeak-chess-whites)))
    (unless all
      (cl-assert
       where t "%s not on board." (emacspeak-chess-piece-name piece)))
    (cond
     (all (emacspeak-chess-collect-all-squares))
     ((or white black)
      (flatten-list (mapcar #'emacspeak-chess-describe-square (sort where '<))))
     (t
      `(
        ,(format "%s %s at"
                 (if color "White " "Black ")
                 (emacspeak-chess-piece-name piece))
        ,@(mapcar  #'emacspeak-chess-square-name (sort where '<)))))))

(defun emacspeak-chess-speak-piece-squares (piece)
  "Prompt for a piece (single char) and speak its locations on the
  board.  Piece is specified as a char using SAN notation. Use
  `w' for all whites pieces,  `l' for all black pieces,
and `a' for entire board.."
  (interactive (list (read-char  "Piece:")))
  (dtk-speak-list (emacspeak-chess-piece-squares piece)))

(defun emacspeak-chess-target-squares (piece)
  "Return a description of pieces that target  current square."
  (cl-declare (special chess-display-index chess-module-game))
  (cl-assert (eq major-mode 'chess-display-mode) t "Not  a Chess display.")
  (cl-assert
   (memq piece
         `(?w ?l
              ,@emacspeak-chess-whites ,@emacspeak-chess-blacks))
   t
   "Specify a piece char, or w for whites and l for blacks")
  (let* ((pos (chess-game-pos chess-module-game chess-display-index))
         (black (= piece ?l))
         (white (= piece ?w))
         (from
          (chess-search-position
           pos
           (get-text-property (point) 'chess-coord)
           (cond
            (black nil)
            (white t)
            (t piece)))))
    (cond
     ((null from)
      (message "Current square not a target for %s"
               (cond
                (black "black")
                (white "white")
                (t (emacspeak-chess-piece-name piece)))))
     (t
      (flatten-list (mapcar #'emacspeak-chess-describe-square (sort from '<)))))))

(defun emacspeak-chess-speak-who-targets (piece)
  "Speak description of squares that can target current square.
Argument `piece' specifies  piece-or-color as in command
  emacspeak-chess-speak-piece-squares"
  (interactive (list (read-char  "Piece:")))
  (dtk-speak-list (emacspeak-chess-target-squares piece)))

(defun emacspeak-chess-view-rank-or-file ()
  "View a complete rank or file from white's perspective."
  (interactive)
  (cl-declare (special last-input-event))
  (let ((f (and (>= last-input-event ?a) (<= last-input-event ?h)))
        (r (and (>= last-input-event ?1) (<= last-input-event ?8)))
        (start nil))
    (setq start
          (chess-coord-to-index
           (cond
            (f  (format "%c1" last-input-event))
            (r  (format "a%c" last-input-event)))))
    (goto-char (chess-display-index-pos (current-buffer) start))
    (cond
     (r (call-interactively #'emacspeak-chess-look-east))
     (f (call-interactively #'emacspeak-chess-look-north))
     (t (error "Rank or file")))))

;;}}}
;;{{{Speaking Moves:
(defvar emacspeak-chess-last-target nil
  "Target square of most recent move.")

(defun emacspeak-chess-describe-move (game &optional move-index)
  "Speak the move by examining game.  Optional argument `move-index'
specifies index of move, default is final index."
  (cl-declare (special emacspeak-chess-whites  emacspeak-chess-last-target))
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
       (color (memq  s-piece emacspeak-chess-whites))
       (t-piece (and target (chess-pos-piece pos target)))
       (promotion (chess-ply-keyword ply :promote)))
    (cond
     ((chess-ply-keyword ply :castle)
      (setq text "short castle"))
     ((chess-ply-keyword ply :long-castle)
      (setq text "long castle"))
     ((and s-piece t-piece (= t-piece ?\ ) target) ;;; target: empty square
      (setq text
            (format
             "%s %s to %s"
             (if color "white " "black ")
             (emacspeak-chess-piece-name s-piece)
             (chess-index-to-coord target))))
     ((and s-piece t-piece target)
      (emacspeak-auditory-icon 'close-object)
      (setq text
            (format
             "%s %s takes %s at %s"
             (if color "white " "black ")
             (emacspeak-chess-piece-name s-piece)
             (emacspeak-chess-piece-name t-piece)
             (chess-index-to-coord target)))))
;; additional consequences of move:
    (when promotion
      (setq text
            (concat
             text ", "
             (format
              "promotes  to %s"
              (emacspeak-chess-piece-name promotion)))))
    (when (chess-ply-keyword ply :en-passant)
      (setq text (concat text ", " "in passing ")))
    (when (chess-ply-keyword ply :check)
      (setq text (concat text ", " "check")))
    (when (chess-ply-keyword ply :checkmate)
      (setq text (concat text ", " "checkmate ")))
    (when (chess-ply-keyword ply :stalemate)
      (setq text (concat text ", " "stalemate ")))
    (when target
      (setq emacspeak-chess-last-target target))
    text))

(defun emacspeak-chess-speak-this-move ()
  "Speak move at current display index."
  (interactive)
  (cl-declare (special chess-module-game chess-display-index))
  (dtk-speak (emacspeak-chess-describe-move chess-module-game
                                            chess-display-index)))

;;}}}
;;{{{ Interactive Commands:
(cl-loop
 for f in
 '(chess-display-search-forward chess-display-search-backward)
 do
 (eval
  `(defadvice ,f (around emacspeak pre act comp)
     "speak."
     (cond
      ((ems-interactive-p)
       (let ((orig chess-display-index))
         ad-do-it
         (when (not (= orig chess-display-index))
           (emacspeak-auditory-icon 'search-hit)
           (dtk-speak  (emacspeak-chess-describe-move chess-module-game chess-display-index)))))
      (t ad-do-it))
     ad-return-value)))

(defun emacspeak-chess-state-speaker  ()
  "Helper function that describes game state."
  (cl-declare (special chess-display-index chess-module-game))
  (let ((msg
         (cond
          ((= chess-display-index (chess-game-index chess-module-game))
           "Current state  ")
          ((= 0 chess-display-index)
           "Start of game ")
          (t (format "Move %d" chess-display-index)))))
    (dtk-speak-and-echo
     (concat
      msg
      (emacspeak-chess-describe-move chess-module-game chess-display-index)))))

(defadvice chess-display-undo  (after emacspeak pre act comp)
  "speak."
  (when (ems-interactive-p)
    (emacspeak-auditory-icon 'progress)
    (dtk-speak (emacspeak-chess-describe-move chess-module-game))))

(defadvice chess-display-move-first (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced ad-do-it)
    (emacspeak-chess-state-speaker)
    (emacspeak-auditory-icon 'left))
   (t ad-do-it))
  ad-return-value)

(defadvice chess-display-move-backward (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced  ad-do-it)
    (emacspeak-auditory-icon 'left)
    (emacspeak-chess-state-speaker))
   (t ad-do-it))
  ad-return-value)

(defadvice chess-display-move-forward (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced ad-do-it)
    (emacspeak-auditory-icon 'right)
    (emacspeak-chess-state-speaker))
   (t ad-do-it))
  ad-return-value)

(defadvice chess-display-move-last (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (ems-with-messages-silenced ad-do-it)
    (emacspeak-auditory-icon 'right)
    (emacspeak-chess-state-speaker))
   (t ad-do-it))
  ad-return-value)

(defadvice chess-display-select-piece (around emacspeak pre act comp)
  "speak."
  (cond
   ((ems-interactive-p)
    (let ((square (get-text-property (point) 'chess-coord)))
      (cond
       ((and
         (consp chess-display-last-selected )
         (= (point) (car chess-display-last-selected)))
        (message "Deselected")
        (emacspeak-auditory-icon 'deselect-object))
       ((null chess-display-last-selected)
        (dtk-speak-list (emacspeak-chess-describe-square square))
        (emacspeak-auditory-icon 'select-object)))
      ad-do-it))
   (t ad-do-it))
  ad-return-value)

;;}}}
;;{{{emacspeak Handler:

(defun chess-emacspeak-handler (game event &rest args)
  "Module chess-emacspeak handler."
  (cond
   ((eq event 'initialize)
    (emacspeak-chess-setup)
    (emacspeak-auditory-icon 'open-object)
    t)
   ((eq event 'move)
    (dtk-speak  (emacspeak-chess-describe-move game))
    (emacspeak-auditory-icon 'time)
    t)
   ((eq event 'kibitz)
    (message (car args)))))

(provide 'chess-emacspeak)
;;}}}
;;{{{Emacspeak Setup:
;; Forward Declaration to help documentation builder.
(defvar chess-default-modules nil)

(defun emacspeak-chess-setup ()
  "Emacspeak setup for Chess."
  (cl-declare (special chess-default-modules chess-display-mode-map))
  (cl-pushnew 'chess-emacspeak chess-default-modules)
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
     ("j" emacspeak-chess-jump)
     ("t" emacspeak-chess-goto-target)
     ("s" emacspeak-chess-speak-piece-squares)
     ("m" emacspeak-chess-speak-this-move)
     ("w" emacspeak-chess-speak-who-targets)
     ("z" emacspeak-chess-speak-board))
   do
   (emacspeak-keymap-update chess-display-mode-map binding)))

;;}}}
(provide 'emacspeak-chess)
;;{{{ end of file

;; local variables:
;; folded-file: t
;; end:

;;}}}
