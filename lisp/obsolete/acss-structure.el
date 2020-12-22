;;; acss-structure.el --- CSS -- Cascaded Speech Style structure  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: DTK Interface for Cascaded Speech Style Sheets
;;; Keywords:emacspeak, audio interface to emacs CSS
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;;Copyright (C) 1995 -- 2018, T. V. Raman 
;;; Copyright (c) 1996 by T. V. Raman
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

;;; Commentary:
;;; The CSS Speech Style Sheet specification defines a number of
;;; abstract device independent voice properties.
;;; A setting conforming to the CSS speech specification can be
;;; represented in elisp as a structure.

;;; We will refer to this structure as a "speech style".  This
;;; structure needs to be mapped to device dependent codes to produce
;;; the desired effect.  This module forms a bridge between emacs
;;; packages    that wish to implement audio formatting 
;;; and Emacspeak's TTS module.  Emacspeak produces voice
;;; change effects by examining the value of text-property
;;; 'personality.

;;; Think of a buffer of formatted text along with the text-property
;;; 'personality appropriately set as a "aural display list".
;;; Module voice-setup.el  help applications like EWW
;;;  produce audio-formatted output by calling  function
;;; acss-personality-from-speech-style  with a  "speech-style"
;;; --a structure as defined in this module and get back a symbol that
;;; they then assign to the value of property 'personality.
;;;Emacspeak's rendering engine then does the needful at the time
;;;speech is produced.
;;; Function acss-personality-from-speech-style does the following:
;;; Takes as input a "speech style"
;;;(1)  Computes a symbol that will be used henceforth to refer to this
;;; specific speech style.
;;; (2) Examines emacspeak's internal voice table to see if this
;;; speech style has a voice already defined.
;;; If so it returns immediately.
;;; Otherwise, it does the additional work of defining a -voice for
;;; future use.
;;; See module voice-setup.el  to see how voices are defined
;;; independent of a given TTS engine.
;;; Code:

;;}}}
;;{{{  Required modules

(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))

;;}}}
;;{{{ Forward Declarations:

(declare-function  tts-voice-defined-p  "dectalk-voices.el" (voice))
(declare-function tts-define-voice-from-speech-style "dectalk-voices.el" (name style))

;;}}}
;;{{{ tts common vars 

(defvar tts-default-voice 'paul 
  "Default voice used. ")

;;}}}
;;{{{  A speech style structure

(cl-defstruct  acss
  family
  gain left-volume right-volume
  average-pitch
  pitch-range
  stress
  richness
  punctuations
  )

;;}}}

(provide  'acss-structure)
;;{{{  emacs local variables

;;; local variables:
;;; folded-file: t
;;; end:

;;}}}
