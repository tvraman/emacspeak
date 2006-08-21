;;; dtk-speak.el --- Provides Emacs Lisp interface to speech server
;;;$Id$
;;; $Author$
;;; Description:  Emacs interface to the dectalk
;;; Keywords: Dectalk Emacs Elisp
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ introduction:

;;; Commentary:
;;;Defines the TTS interface.

;;; Code:
;; 

;;}}}
;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'backquote)
(require 'custom)
(require 'dtk-tcl)

;;}}}
;;{{{  user customizations:

(defgroup tts nil
  "Text To Speech (TTS) customizations for the Emacspeak audio desktop."
  :group 'emacspeak
  :prefix "dtk-")

(defcustom dtk-stop-immediately-while-typing t
  "*Set it to nil if you dont want speech to flush as you
type.  You can use command
`dtk-toggle-stop-immediately-while-typing' bound to
\\[dtk-toggle-stop-immediately-while-typing] to toggle this setting."
  :group 'tts
  :type 'boolean)
(defcustom dtk-speech-rate-base 50
  "*Value of lowest tolerable speech rate."
  :type 'integer
  :group 'tts)

(defcustom dtk-speech-rate-step 50
  "*Value of speech rate increment.
This determines step size used when setting speech rate via command
`dtk-set-predefined-speech-rate'.  Formula used is
dtk-speech-rate-base  +  dtk-speech-rate-step*level."
  :type 'integer
  :group 'tts)

(defcustom dtk-startup-hook nil
  "List of hooks to be run after starting up the speech server.  
Set things like speech rate, punctuation mode etc in this
hook."
  :type 'hook)

(defvar dtk-tcl (or (getenv "DTK_TCL" )
                    "tcl")
  "Interpreter  used to run the speech server.
Extended tcl --tcl-- for all of the currently available servers.")

(defvar dtk-program
  (or  (getenv "DTK_PROGRAM" ) "dtk-exp")
  "The program to use to talk to the speech engine.
Possible choices at present:
dtk-exp     For the Dectalk Express.
dtk-mv      for the Multivoice and older Dectalks.
outloud     For IBM ViaVoice Outloud
The default is dtk-exp.")

;;}}}
;;{{{  Internal variables:

(defvar dtk-stop-immediately t
  "If t, speech stopped immediately when new speech received.
Emacspeak sets this to nil if the current message being spoken is too
important to be interrupted.")

(defvar dtk-speaker-process nil
  "Speaker process handle.")

(defvar dtk-punctuation-mode  "some"
  "Current setting of punctuation state.
Possible values are some, all or none.
You should not modify this variable;
Use command  `dtk-set-punctuations' bound to
\\[dtk-set-punctuations].  .")

;;; forward declaration 
(defvar emacspeak-servers-directory
  (expand-file-name
   "servers/"
   emacspeak-directory))

(defun tts-setup-servers-alist ()
  "Sets up tts servers alist from file servers/.servers. 
File .servers is expected to contain name of one server per
no line --with no white space."
  (declare (special emacspeak-servers-directory
                    dtk-servers-alist))
  (let ((result nil)
        (start nil)
        (scratch (get-buffer-create " *servers*"))
        (this nil))
    (save-excursion
      (set-buffer scratch)
      (erase-buffer)
      (insert-file
       (expand-file-name ".servers"
                         emacspeak-servers-directory))
      (goto-char (point-min))
      (goto-char (point-min))
      (while (not (eobp))
        (setq start (point))
        (unless 
            (looking-at  "^#")
          (end-of-line)
          (setq this (buffer-substring-no-properties start (point)))
          (push
           (cons this this)
           result))
        (forward-line 1)))
    (setq dtk-servers-alist result)))

(defvar dtk-servers-alist
  nil
  "Used by `completing-read' when prompting for the dtk
server to use.
This variable is automatically setup to reflect the
available TTS servers.")

;;}}}
;;{{{ macros

(defmacro tts-with-punctuations (setting &rest body)
  "Safely set punctuation mode for duration of body form."
  (`
   (progn
     (declare (special dtk-punctuation-mode))
     (let    ((save-punctuation-mode dtk-punctuation-mode))
       (unwind-protect
           (progn
             (unless (string= (, setting) save-punctuation-mode)
               (process-send-string dtk-speaker-process
                                    (format "tts_set_punctuations %s  \n "
                                            (, setting)))
               (setq dtk-punctuation-mode (, setting)))
             (,@ body)
             (dtk-force))
         (unless (string=  (, setting)  save-punctuation-mode)
           (setq dtk-punctuation-mode save-punctuation-mode)
           (process-send-string dtk-speaker-process
                                (format "tts_set_punctuations %s  \n "
                                        dtk-punctuation-mode ))
           (dtk-force)))))))

;;}}}
;;{{{  Mapping characters to speech:

;;{{{ variable to hold buffer specific speech table

(defvar dtk-display-table nil
  "Variable holding display information for special characters.")

(make-variable-buffer-local 'dtk-display-table)

;;}}}
;;{{{  default speech table

(defvar dtk-character-to-speech-table
  (make-vector 256 "")
  "Maps characters to pronunciation strings.")
(declaim (special dtk-character-to-speech-table ))

;;;  Assign entries in the table:
(defun dtk-speak-setup-character-table ()
  "Setup pronunciations in the character table for the Dectalk."
  (let ((table dtk-character-to-speech-table))
    (aset  table 0 "control at")
    (aset  table 1 "control a")
    (aset  table 2 "control b")
    (aset  table 3  "control c")
    (aset  table 4 "control d")
    (aset  table 5 "control e")
    (aset  table 6 "control f")
    (aset  table 7 "control g")
    (aset  table 8 "control h")
    (aset  table 9 "tab")
    (aset  table 10 "newline")
    (aset  table 11 "control k")
    (aset  table 12 "control l")
    (aset  table 13 "control m")
    (aset  table 14 "control n")
    (aset  table 15 "control o")
    (aset  table 16"control p")
    (aset  table 17 "control q")
    (aset  table 18 "control r")
    (aset  table 19 "control s")
    (aset  table 20 "control t")
    (aset  table 21 "control u")
    (aset  table 22 "control v")
    (aset  table 23 "control w")
    (aset  table 24 "control x")
    (aset  table 25 "control y")
    (aset  table 26 "control z")
    (aset  table 27 "escape")
    (aset table 28 "control[*]backslash")
    (aset table 29 "control[*]right bracket")
    (aset table 30 "control[*]caret" )
    (aset table 31 "control[*]underscore")
    (aset table 32 "space")
    (aset table 33 "exclamation")
    (aset table 34 "quotes")
    (aset table 35  "pound")
    (aset table  36  "dollar")
    (aset table 37  "percent" )
    (aset table 38  "ampersand")
    (aset table 39  "apostrophe" )
    (aset table 40  "left[*]paren" )
    (aset table 41 "right[*]paren" )
    (aset table  42   "star")
    (aset table 43  "plus")
    (aset   table 44 "comma")
    (aset table  45  "dash")
    (aset table 46  "dot")
    (aset table 47  "slash")
    (aset table 48  "zero")
    (aset table 49 "one")
    (aset table 50 "two")
    (aset table 51 "three")
    (aset  table 52  "four")
    (aset  table 53 "five")
    (aset  table 54 "six")
    (aset  table 55 "seven")
    (aset  table 56 "eight")
    (aset  table 57 "nine")
    (aset table 58 "colon" )
    (aset table 59 "semi")
    1(aset table 60 "less[*]than")
    (aset  table 61 "equals")
    (aset  table 62  "greater[*]than")
    (aset  table 63 "question[*]mark")
    (aset  table 64 "at")
    (aset  table 65  " cap[*]a")
    (aset  table 66 " cap[*]b")
    (aset  table 67 "cap[*]c")
    (aset  table 68 "cap[*]d")
    (aset  table 69 "cap[*]e")
    (aset  table 70 "cap[*]f")
    (aset  table 71 "cap[*]g")
    (aset  table 72 "cap[*]h")
    (aset  table 73 "cap[*]i")
    (aset  table 74 "cap[*]j")
    (aset  table 75 "cap[*]k")
    (aset  table 76 "cap[*]l")
    (aset  table 77 "cap[*]m")
    (aset  table 78 "cap[*]m")
    (aset  table 79 "cap[*]o")
    (aset  table 80 "cap[*]p")
    (aset  table 81 "cap[*]q")
    (aset  table 82 "cap[*]r")
    (aset  table 83 "cap[*]s")
    (aset  table 84 "cap[*]t")
    (aset  table 85 "cap[*]u")
    (aset  table 86 "cap[*]v")
    (aset  table 87 "cap[*]w")
    (aset  table 88 "cap[*]x")
    (aset  table 89 "cap[*]y")
    (aset  table 90 "cap[*]z")
    (aset  table 91 "left[*]bracket")
    (aset  table 92  "backslash")
    (aset  table 93 "right[*]bracket")
    (aset  table 94  "caret")
    (aset  table 95  "underscore")
    (aset  table 96 "backquote")
    (aset  table 97  "a")
    (aset  table 98 "b")
    (aset  table 99 "c")
    (aset  table 100 "d")
    (aset  table 101 "e")
    (aset  table 102 "f")
    (aset  table 103 "g")
    (aset  table 104 "h")
    (aset  table 105 "i")
    (aset  table 106 "j")
    (aset  table 107 "k")
    (aset  table 108 "l")
    (aset  table 109 "m")
    (aset  table 110 "n")
    (aset  table 111 "o")
    (aset  table 112 "p")
    (aset  table 113 "q")
    (aset  table 114 "r")
    (aset  table 115 "s")
    (aset  table 116 "t")
    (aset  table 117 "u")
    (aset  table 118 "v")
    (aset  table 119 "w")
    (aset  table 120  "x")
    (aset  table 121 "y")
    (aset  table 122 "z")
    (aset  table 123 "left[*]brace")
    (aset  table 124 "pipe")
    (aset  table 125 "right[*]brace ")
    (aset  table 126 "tilde")
    (aset  table 127  "backspace")
;;; Characters with the 8th bit set:
    (aset  table 128  " octal 200 ")
    (aset  table 129  " ")              ;shows up on WWW pages
    (aset  table 130  " octal 202 ")
    (aset  table 131  " octal 203 ")
    (aset  table 132  " octal 204 ")
    (aset  table 133  " octal 205 ")
    (aset  table 134  " octal 206 ")
    (aset  table 135  " octal 207 ")
    (aset  table 136  " octal 210 ")
    (aset  table 137  " octal 211 ")
    (aset  table 138  " octal 212 ")
    (aset  table 139  " octal 213 ")
    (aset  table 140  " octal 214 ")
    (aset  table 141  " octal 215 ")
    (aset  table 142  " octal 216 ")
    (aset  table 143  " octal 217 ")
    (aset  table 144  " octal 220 ")
    (aset  table 145  " octal 221 ")
    (aset  table 146  " '  ")
    (aset  table 147  " quote  ")
    (aset  table 148  " octal 224 ")
    (aset  table 149  " octal 225 ")
    (aset  table 150  " octal 226 ")
    (aset  table 151  " octal 227 ")
    (aset  table 152  " octal 230 ")
    (aset  table 153  " octal 231 ")
    (aset  table 154  " octal 232 ")
    (aset  table 155  " octal 233 ")
    (aset  table 156  " octal 234 ")
    (aset  table 157  " octal 235 ")
    (aset  table 158  " octal 236 ")
    (aset  table 159  " octal 237 ")
    (aset  table 160  "  ")             ;non breaking space
    (aset  table 161  " octal 241 ")
    (aset  table 162  " octal 242 ")
    (aset  table 163  " octal 243 ")
    (aset  table 164  " octal 244 ")
    (aset  table 165  " octal 245 ")
    (aset  table 166  " octal 246 ")
    (aset  table 167  " octal 247 ")
    (aset  table 168  " octal 250 ")
    (aset  table 169  " copyright ")    ;copyright sign
    (aset  table 170  " octal 252 ")
    (aset  table 171  " octal 253 ")
    (aset  table 172  " octal 254 ")
    (aset  table 173  "-")              ;soft hyphen
    (aset  table 174  " (R) ")          ;registered sign
    (aset  table 175  " octal 257 ")
    (aset  table 176  " octal 260 ")
    (aset  table 177  " octal 261 ")
    (aset  table 178  " octal 262 ")
    (aset  table 179  " octal 263 ")
    (aset  table 180  " octal 264 ")
    (aset  table 181  " octal 265 ")
    (aset  table 182  " octal 266 ")
    (aset  table 183  " octal 267 ")
    (aset  table 184  " octal 270 ")
    (aset  table 185  " octal 271 ")
    (aset  table 186  " octal 272 ")
    (aset  table 187  " octal 273 ")
    (aset  table 188  " octal 274 ")
    (aset  table 189  " octal 275 ")
    (aset  table 190  " octal 276 ")
    (aset  table 191  " octal 277 ")
    (aset  table 192  " octal 300 ")
    (aset  table 193  " octal 301 ")
    (aset  table 194  " octal 302 ")
    (aset  table 195  " octal 303 ")
    (aset  table 196  " octal 304 ")
    (aset  table 197  " octal 305 ")
    (aset  table 198  " octal 306 ")
    (aset  table 199  " octal 307 ")
    (aset  table 200  " octal 310 ")
    (aset  table 201  " octal 311 ")
    (aset  table 202  " octal 312 ")
    (aset  table 203  " octal 313 ")
    (aset  table 204  " octal 314 ")
    (aset  table 205  " octal 315 ")
    (aset  table 206  " octal 316 ")
    (aset  table 207  " octal 317 ")
    (aset  table 208  " octal 320 ")
    (aset  table 209  " octal 321 ")
    (aset  table 210  " octal 322 ")
    (aset  table 211  " octal 323 ")
    (aset  table 212  " octal 324 ")
    (aset  table 213  " octal 325 ")
    (aset  table 214  " octal 326 ")
    (aset  table 215  " octal 327 ")
    (aset  table 216  " octal 330 ")
    (aset  table 217  " octal 331 ")
    (aset  table 218  " octal 332 ")
    (aset  table 219  " octal 333 ")
    (aset  table 220  " octal 334 ")
    (aset  table 221  " octal 335 ")
    (aset  table 222  " octal 336 ")
    (aset  table 223  " octal 337 ")
    (aset  table 224  " octal 340 ")
    (aset  table 225  " octal 341 ")
    (aset  table 226  " octal 342 ")
    (aset  table 227  " octal 343 ")
    (aset  table 228  " octal 344 ")
    (aset  table 229  " octal 345 ")
    (aset  table 230  " octal 346 ")
    (aset  table 231  " octal 347 ")
    (aset  table 232  " octal 350 ")
    (aset  table 233  " octal 351 ")
    (aset  table 234  " octal 352 ")
    (aset  table 235  " octal 353 ")
    (aset  table 236  " octal 354 ")
    (aset  table 237  " octal 355 ")
    (aset  table 238  " octal 356 ")
    (aset  table 239  " octal 357 ")
    (aset  table 240  " octal 360 ")
    (aset  table 241  " octal 361 ")
    (aset  table 242  " octal 362 ")
    (aset  table 243  " octal 363 ")
    (aset  table 244  " octal 364 ")
    (aset  table 245  " octal 365 ")
    (aset  table 246  " octal 366 ")
    (aset  table 247  " octal 367 ")
    (aset  table 248  " octal 370 ")
    (aset  table 249  " octal 371 ")
    (aset  table 250  " octal 372 ")
    (aset  table 251  " octal 373 ")
    (aset  table 252  " octal 374 ")
    (aset  table 253  " octal 375 ")
    (aset  table 254  " octal 376 ")
    (aset  table 255  " octal 377 ")))

(dtk-speak-setup-character-table)
;;}}}
;;{{{  iso ascii table:

(defvar dtk-iso-ascii-character-to-speech-table
  (and (boundp 'dtk-character-to-speech-table)
       (vectorp dtk-character-to-speech-table)
       (copy-sequence dtk-character-to-speech-table))
  "Table that records how ISO ascii characters are spoken.")

(let ((table dtk-iso-ascii-character-to-speech-table))
  (aset table 160 " no-break space ")
  (aset table 161 " inverted exclamation mark ")
  (aset table 162 " cent sign ")
  (aset table 163 " sterling ")
  (aset table 164 " general currency sign ")
  (aset table 165 " yen sign ")
  (aset table 166 " broken vertical line ")
  (aset table 167 " section sign ")
  (aset table 168 " diaeresis ")
  (aset table 169 " copyright sign ")
  (aset table 170 " ordinal indicator, feminine ")
  (aset table 171 " left angle quotation mark ")
  (aset table 172 " not sign ")
  (aset table 173 " soft hyphen ")
  (aset table 174 " registered sign ")
  (aset table 175 " macron ")
  (aset table 176 " degree sign ")
  (aset table 177 " plus or minus sign ")
  (aset table 178 " superscript two ")
  (aset table 179 " superscript three ")
  (aset table 180 " acute ")
  (aset table 181 " mu ")
  (aset table 182 " pilcrow ")
  (aset table 183 " middle dot ")
  (aset table 184 " cedilla ")
  (aset table 185 " superscript one ")
  (aset table 186 " ordinal indicator, masculine ")
  (aset table 187 " right angle quotation mark ")
  (aset table 188 " fraction one-quarter ")
  (aset table 189 " fraction one-half ")
  (aset table 190 " fraction three-quarters ")
  (aset table 191 " inverted question mark ")
  (aset table 192 " A graav ")
  (aset table 193 " A acute ")
  (aset table 194 " A circumflex ")
  (aset table 195 " A tilde ")
  (aset table 196 " A diaeresis ")
  (aset table 197 " A ring ")
  (aset table 198 " AE diphthong ")
  (aset table 199 " C cedilla ")
  (aset table 200 " E graav ")
  (aset table 201 " E acute ")
  (aset table 202 " E circumflex ")
  (aset table 203 " E diaeresis ")
  (aset table 204 " I graav ")
  (aset table 205 " I acute ")
  (aset table 206 " I circumflex ")
  (aset table 207 " I diaeresis ")
  (aset table 208 " D stroke, Icelandic eth ")
  (aset table 209 " N tilde ")
  (aset table 210 " O graav ")
  (aset table 211 " O acute ")
  (aset table 212 " O circumflex ")
  (aset table 213 " O tilde ")
  (aset table 214 " O diaeresis ")
  (aset table 215 " multiplication sign ")
  (aset table 216 " O slash ")
  (aset table 217 " U graav ")
  (aset table 218 " U acute ")
  (aset table 219 " U circumflex ")
  (aset table 220 " U diaeresis ")
  (aset table 221 " Y acute ")
  (aset table 222 " capital thorn, Icelandic ")
  (aset table 223 " small sharp s, German ")
  (aset table 224 " a graav ")
  (aset table 225 " a acute ")
  (aset table 226 " a circumflex ")
  (aset table 227 " a tilde ")
  (aset table 228 " a diaeresis ")
  (aset table 229 " a ring ")
  (aset table 230 " ae diphthong ")
  (aset table 231 " c cedilla ")
  (aset table 232 " e graav ")
  (aset table 233 " e acute ")
  (aset table 234 " e circumflex ")
  (aset table 235 " e diaeresis ")
  (aset table 236 " i graav ")
  (aset table 237 " i acute ")
  (aset table 238 " i circumflex ")
  (aset table 239 " i diaeresis ")
  (aset table 240 " d stroke, Icelandic eth ")
  (aset table 241 " n tilde ")
  (aset table 242 " o graav ")
  (aset table 243 " o acute ")
  (aset table 244 " o circumflex ")
  (aset table 245 " o tilde ")
  (aset table 246 " o diaeresis ")
  (aset table 247 " division sign ")
  (aset table 248 " o slash ")
  (aset table 249 " u graav ")
  (aset table 250 " u acute ")
  (aset table 251 " u circumflex ")
  (aset table 252 " u diaeresis ")
  (aset table 253 " y acute ")
  (aset table 254 " small thorn, Icelandic ")
  (aset table 255 " small y diaeresis ")
  )

;;}}}
(defsubst dtk-char-to-speech (char)
  "Translate CHAR to speech string."
  (declare (special dtk-character-to-speech-table))
  (if (> char 127 )
      (format "octal %o"  char )
    (aref dtk-character-to-speech-table char )))

;;}}}
;;{{{  interactively selecting the server:

(defvar tts-voice-reset-code nil
  "Code sent to reset the voice to its default.
This is setup on a per engine basis.")

;;; will be reset on a per TTS engine basis.
(defalias 'tts-get-voice-command 'dtk-get-voice-command)
  
(defun tts-configure-synthesis-setup (&optional tts-name)
  "Setup synthesis environment. "
  (declare (special dtk-default-speech-rate
                    tts-default-speech-rate
                    outloud-default-speech-rate
                    emacspeak-aumix-multichannel-capable-p emacspeak-aumix-midi-available-p emacspeak-use-auditory-icons
                    dtk-program
                    ))
  (unless tts-name
    (setq tts-name dtk-program))
  (cond
   ((string-match "outloud" tts-name)
    (require 'outloud-voices)
    (require 'outloud-css-speech)
    (fset 'tts-get-voice-command 'outloud-get-voice-command)
    (fset 'dtk-personality-from-speech-style
          'outloud-personality-from-speech-style)
    (setq tts-default-speech-rate outloud-default-speech-rate))
   (t (require 'dtk-voices)
      (fset 'tts-get-voice-command 'dtk-get-voice-command)
      (fset 'dtk-personality-from-speech-style
            'dectalk-personality-from-speech-style)
      (setq tts-default-speech-rate dtk-default-speech-rate)))
  (setq tts-voice-reset-code (tts-get-voice-command
                              tts-default-voice))
  (when (and (string= "outloud" dtk-program)
             emacspeak-use-auditory-icons
             (not emacspeak-aumix-multichannel-capable-p)
             (not (emacspeak-using-midi-p))
             emacspeak-aumix-midi-available-p)
    (emacspeak-set-auditory-icon-player 'emacspeak-midi-icon)))

;;; forward declaration.
(defun dtk-select-server (program )
  "Select a speech server interactively.
Argument PROGRAM specifies the speech server program.
When called  interactively, The selected server is started immediately. "
  (interactive
   (list
    (completing-read
     "Select speech server:"
     (or dtk-servers-alist
         (progn
           (tts-setup-servers-alist)
           dtk-servers-alist))
     nil
     t  )))
  (declare (special  dtk-tcl dtk-program dtk-servers-alist))
  (setq dtk-program program)
  (tts-configure-synthesis-setup dtk-program)
  (when (interactive-p)
    (dtk-initialize)))

;;}}}
;;{{{  initialize the speech process

(defvar dtk-debug nil
  "Set this to t if you want to debug the synthesizer server.")

(defvar dtk-speak-server-initialized nil
  "Records if the server is initialized.")

(defvar dtk-speak-process-connection-type nil
  "*Specifies if we use ptys or pipes to connect to the speech server process.
Has the same semantics as the builtin `process-connection-type'.
Default is to use pipes.")

(defvar tts-debug-buffer " *speaker*"
  "Buffer holding speech server debug output.")

(defun  dtk-initialize ()
  "Initialize speech system."
  (declare (special dtk-program dtk-tcl
                    tts-debug-buffer dtk-speak-process-connection-type
                    dtk-speaker-process  dtk-debug
                    dtk-speak-server-initialized
                    dtk-startup-hook emacspeak-servers-directory))
  (let ((new-process nil)
        (process-connection-type  dtk-speak-process-connection-type))
    (setq new-process
          (apply 'start-process
                 "speaker"
                 (and dtk-debug tts-debug-buffer)
                 dtk-tcl
                 (list
                  (expand-file-name dtk-program
                                    emacspeak-servers-directory))))
    (process-kill-without-query new-process)
    (setq dtk-speak-server-initialized
          (or (eq 'run (process-status new-process ))
              (eq 'open (process-status new-process))))
    (cond
     (dtk-speak-server-initialized
      ;; nuke old server
      (when (and dtk-speaker-process
                 (or (eq 'run (process-status dtk-speaker-process ))
                     (eq 'open (process-status dtk-speaker-process ))
                     (eq 'stop (process-status dtk-speaker-process ))))
        (delete-process dtk-speaker-process ))
      (setq dtk-speaker-process new-process)
      (tts-configure-synthesis-setup dtk-program)
      (run-hooks 'dtk-startup-hook ))
     (t 
      (message "The speech server is not running.")))))

(defun tts-restart ()
  "Use this to nuke the currently running TTS server and restart it."
  (interactive)
  (dtk-initialize ))

(defun tts-show-debug-buffer ()
  "Select TTS debugging buffer."
  (interactive)
  (declare (special tts-debug-buffer))
  (switch-to-buffer tts-debug-buffer))
  

(defun dtk-toggle-debug (&optional flag )
  "Toggle state of the debug FLAG.
When debugging is on, you can switch to the buffer
*speaker* to examine the output from the process
that talks to the speech device by using command \\[tts-show-debug-buffer].
Note: *speaker* is a hidden buffer, ie it has a leading space in its name."
  (interactive "P")
  (declare (special dtk-debug ))
  (cond
   (flag (setq dtk-debug t ))
   (t (setq dtk-debug (not dtk-debug ))))
  (message "Turned %s debugging of the synthesizer server %s"
           (if dtk-debug "on" "off")
           (if dtk-debug
               (substitute-command-keys
                "Restart the synthesizer server  by pressing
\\[dtk-emergency-restart] to start debugging")
             (substitute-command-keys
              "Restart the synthesizer server  by pressing
\\[dtk-emergency-restart] to stop  debugging"))))

;;}}}
;;{{{  interactively select how text is split:

(defun dtk-toggle-splitting-on-white-space ()
  "Toggle splitting of speech on white space.
This affects the internal state of emacspeak that decides if we split
text purely by clause boundaries, or also include
whitespace.  By default, emacspeak sends a clause at a time
to the speech device.  This produces fluent speech for
normal use.  However in modes such as `shell-mode' and some
programming language modes, clause markers appear
infrequently, and this can result in large amounts of text
being sent to the speech device at once, making the system
unresponsive when asked to stop talking.  Splitting on white
space makes emacspeak's stop command responsive.  However,
when splitting on white space, the speech sounds choppy
since the synthesizer is getting a word at a time."
  (interactive)
  (declare (special dtk-chunk-separator-syntax))
  (cond
   ((not (string-match " " dtk-chunk-separator-syntax))
    (dtk-chunk-on-white-space-and-punctuations)
    (message "Text will be split at punctuations and white space when speaking") )
   (t (dtk-chunk-only-on-punctuations)
      (message "Text split only at clause boundaries when
speaking"))))

(defun dtk-set-chunk-separator-syntax (s)
  "Interactively set how text is split in chunks.
See the Emacs documentation on syntax tables for details on how characters are
classified into various syntactic classes.
Argument S specifies the syntax class."

  (interactive
   (list
    (read-from-minibuffer "Specify separator syntax string: ")))
  (declare (special dtk-chunk-separator-syntax))
  (setq dtk-chunk-separator-syntax s)
  (message "Set  separator to %s" s))

;;}}}
(provide 'dtk-speak)

;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; byte-compile-dynamic: nil
;;; end:

;;}}}
