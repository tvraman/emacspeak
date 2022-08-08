;; Midge mode - for writing midge(1) source files

(require 'compile)
(require 'regexp-opt)
(require 'skeleton)

;;; Code:

(defvar midge-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "}" 'midge-close-bracket)
    (define-key map "\t" 'midge-indent-line)

    (define-key map "\C-c\C-ch" 'midge-head-block)
    (define-key map "\C-c\C-cg" 'midge-body-block)
    (define-key map "\C-c\C-cn" 'midge-channel-block)
    (define-key map "\C-c\C-cr" 'midge-repeat-block)
    (define-key map "\C-c\C-cb" 'midge-bend-block)
    (define-key map "\C-c\C-cd" 'midge-define-block)
    (define-key map "\C-c\C-cc" 'midge-choose-block)
    (define-key map "\C-c\C-ca" 'midge-chain-block)
    (define-key map "\C-c\C-ct" 'midge-tuplet-block)
    (define-key map "\C-c\C-dr" 'midge-repeat-line)
    (define-key map "\C-c\C-db" 'midge-bend-line)
    (define-key map "\C-c\C-dd" 'midge-define-line)
    (define-key map "\C-c\C-dc" 'midge-choose-line)
    (define-key map "\C-c\C-dt" 'midge-tuplet-line)

    (define-key map "\C-c\C-fp" 'midge-select-patch)
    (define-key map "\C-c\C-fd" 'midge-select-drum)
    (define-key map "\C-c\C-fs" 'midge-select-scale)

    (define-key map "\C-c\C-ft" 'midge-insert-tempo)
    (define-key map "\C-c\C-fg" 'midge-insert-time-sig)
    (define-key map "\C-c\C-fv" 'midge-insert-volume)
    (define-key map "\C-c\C-fy" 'midge-insert-pan)
    (define-key map "\C-c\C-fr" 'midge-insert-reverb)
    (define-key map "\C-c\C-fc" 'midge-insert-chorus)

    (define-key map "\C-c\C-v\C-c" 'midge-compile)
    (define-key map "\C-c\C-v\C-b" 'midge-compile-background)
    (define-key map "\C-c\C-v\C-v" 'midge-compile-verbose)
    (define-key map "\C-c\C-v\C-d" 'midge-compile-debug)
    (define-key map "\C-c\C-v\C-f" 'midge-compile-ask)

    (define-key map "\C-c\C-v\C-m" 'midge-decompile)

    (define-key map "\C-c\C-v\C-p" 'midge-play-background)
    (define-key map "\C-c\C-v\C-l" 'midge-play-foreground)
    (define-key map "\C-c\C-v\C-o" 'midge-play-ask-background)
    (define-key map "\C-c\C-v\C-k" 'midge-play-ask-foreground)
    map)
  "Local keymap for midge mode buffers.")

(defconst midge-drum-names
  '("agogo_h" "agogo_l" "bd" "bd_ac" "bongo_h" "bongo_l" "cabasa" "clap"
    "claves" "conga_h_mute" "conga_h_open" "conga_l" "cowbell" "cuica_mute"
    "cuica_open" "cym_chinese" "cym_crash" "cym_crash_2" "cym_ride"
    "cym_ride_2" "cym_splash" "ftom_h" "ftom_l" "guiro_lg" "guiro_sh" "hh_c"
    "hh_o" "hh_p" "maracas" "ride_bell" "sd_ac" "sd_el" "stick" "tamb"
    "timbale_h" "timbale_l" "tom_h" "tom_hm" "tom_l" "tom_lm" "tri_mute"
    "tri_open" "vibraslap" "whistle_lg" "whistle_sh" "wood_h" "wood_l"))

(defconst midge-instrument-names
  '("accordian" "accordian_tango" "agogo" "bagpipe" "banjo"
    "bass_ac" "bass_fg" "bass_fless" "bass_pick" "bass_slap_1" "bass_slap_2"
    "bass_syn_1" "bass_syn_2" "bassoon" "bell_tinkle" "bottle" "brass"
    "brass_syn_1" "brass_syn_2" "celesta" "cello" "choir_aahs" "clarinet"
    "clavinet" "contrabass" "cymbal_rev" "drum_steel" "drum_syn" "drum_taiko"
    "dulcimer" "fiddle" "flute" "flute_pan" "fx_atmos" "fx_breath" "fx_bright"
    "fx_copter" "fx_crystal" "fx_echo" "fx_fret" "fx_goblin" "fx_gun"
    "fx_phone" "fx_rain" "fx_scifi" "fx_sea" "fx_strack" "fx_tweeet"
    "glockenspiel" "guitar_clean" "guitar_dist" "guitar_harm" "guitar_jazz"
    "guitar_muted" "guitar_nylon" "guitar_od" "guitar_steel" "harmonica"
    "harpsichord" "hornen" "hornfr" "kalimba" "koto" "lead_basslead"
    "lead_calliope" "lead_charang" "lead_chiff" "lead_fifth" "lead_saw"
    "lead_sq" "lead_voice" "marimba" "music_box" "oboe" "ocarina" "orch_hit"
    "organ_church" "organ_dbar" "organ_perc" "organ_reed" "organ_rock"
    "pad_bowed" "pad_choir" "pad_halo" "pad_metal" "pad_new_age" "pad_sweep"
    "pad_warm" "piano_br" "piano_el_1" "piano_el_2" "piano_grand_ac"
    "piano_grand_el" "piano_ht" "piccolo" "polysynth" "recorder" "saxalt"
    "saxbar" "saxsop" "saxten" "shamisen" "shenai" "sitar" "skakuhachi"
    "str_ens_1" "str_ens_2" "str_orch" "str_pizz" "str_syn_1" "str_syn_2"
    "str_trem" "timpani" "tom_melodic" "trombone" "trombone_muted" "trumpet"
    "tuba" "tubular_bells" "vibraphone" "viola" "violin" "voice_oohs"
    "voice_syn" "whistle" "woodblock" "xylophone"))

(defconst midge-scale-names
  '("aeolian" "arabian" "bebop" "bebop_dorian" "bebop_mixolydian" "chromatic"
    "dorian" "gypsy" "ionian" "locrian" "lydian" "major" "major_pentatonic"
    "minor" "minor_harmonic" "minor_jazz" "minor_pentatonic" "mixolydian"
    "phrygian" "spanish" "whole_tone"))

(defconst midge-simple-keywords '("attack" "bank" "bend_range" "chorus" "ctrl"
				  "decay" "key" "key_strict" "length" "marker"
				  "nrpn" "octave" "pan" "patch" "pitch"
				  "print" "resolution" "reverb" "rpn"
				  "shorten" "strum" "tempo" "time_sig" "title"
				  "unquantise" "volume")
  "Simple midge keywords.
All strings in this list should be prefixed with '$'.")

;; syntax highlighting
(defvar midge-font-lock-keywords
  `(; Main blocks
    ("\\@\\(head\\|body\\|channel\\)" 1 font-lock-builtin-face)

    ; Unquoted track title
    ("\\@channel[[:blank:]]+[0-9]+[[:blank:]]+\\([^{[:blank:]]+\\)\\([[:blank:]]\\|$\\)" 1 font-lock-string-face)

    ; "-" before "{" modifies the behaviour of the block.
    ("[[:blank:]]+\\(-\\)[[:blank:]]+{" 1 font-lock-warning-face)

    ; Note on and note off
    ("\\(^\\|[[:blank:]]\\)\\([-+]\\)" 2 font-lock-warning-face)

    ; Block keywords
    ("\\%\\(repeat\\|pan_all\\|bend\\|choose\\|chain\\|eval\\|include\\|tuplet\\|verbatim\\|bytes\\)" 1 font-lock-function-name-face)
    ("\\%\\(define\\)[[:blank:]]+\\([^[:blank:]]\\)+" 1 font-lock-function-name-face)
    ("\\(\\%define\\)[[:blank:]]+\\([^[:blank:]]+\\)" 2 font-lock-variable-name-face)

    ; Simple keywords
    (,(concat "\\$\\("
	      (regexp-opt midge-simple-keywords)
	      "\\)") 1 font-lock-keyword-face)

    ; Unquoted marker or title text
    ("\\$\\(marker\\|title\\)[[:blank:]]+\\([^[:blank:]]+\\)\\([[:blank:]]\\|$\\)" 2 font-lock-string-face)

    ; Riff names
    ("\\~\\([^/[:blank:]]+\\)" 1 font-lock-variable-name-face)

    ; Scales
    (,(concat "\\(^\\| \\|\t\\)\\("
	      (regexp-opt midge-scale-names)
	      "\\)\\($\\| \\|\t\\)") . font-lock-reference-face)

    ; Drum names
    (,(concat "\\(^\\|[/[:blank:]]\\)\\("
	      (regexp-opt midge-drum-names)
	      "\\)") 2 font-lock-constant-face)

    ; Instrument names
    (,(concat "\\$patch[[:blank:]]+[,0-9]*\\("
	      (regexp-opt midge-instrument-names)
	      "\\)\\([[:blank:]]\\|$\\)") 1 font-lock-constant-face)

    ; Hex values
    ("\\(^\\|[[:blank:]]\\)\\(0x[0-9]+\\)\\([[:blank:]]\\|$\\)" 2 font-lock-string-face)

    ; Ranges, bend values etc
    ("\\(^\\|:\\|[[:blank:]]\\)[0-9]+\\([-+x]\\)[0-9]+\\([[:blank:]]\\|$\\)" 2 font-lock-warning-face)

    ; Transpose numbers
    ("/\\(-?[0-9]+\\)/" 1 font-lock-type-face)

    ; Note options
    ("\\(\\(^\\|[[:blank:]]\\|[-+]\\)/\\|[0-9]\\)\\([a-yA-Y]\\|[zZ][-+]?\\)" 3 font-lock-keyword-face)

    ; Keywords inside blocks
    ("\\(scale\\|rhythm\\|times\\|start\\)" . font-lock-builtin-face)

    ; Start and end of chords
    ("[()]" . font-lock-warning-face)

    ; Simple bend syntax
    ("=>" . font-lock-warning-face)

    ; "[length]x[repeat]" in rhythm blocks
;    ("\\(^\\|[[:blank:]]\\)[0-9]+\\(x\\)[0-9]+\\([[:blank:]]\\|$\\)" 2 font-lock-warning-mode)

    ; Ranges
;    ("\\$[^[:blank:]]+[[:blank:]]\\([0-9]+-[0-9]+\\)" 2 font-lock-string-face)

    ; Bend values
;    ("\\(^\\|[[:blank:]]\\)[0-9]+\\([-+]\\)[0-9]+\\([[:blank:]]\\|$\\)" 2 font-lock-warning-face)

    ; Numbers and time values
;    ("\\(^\\|[,[:blank:]]\\)\\([0-9]+\\([:/][0-9]+\\)?\\)" 2 font-lock-string-face)

    ; Comma separated lists
;    ("[a-zA-Z0-9]+\\(,\\)" 1 font-lock-warning-face)

    ; Fractional time values
;    ("[0-9]+\\(:\\)[0-9]+" 1 font-lock-warning-face)

  )
  "Highlighting expressions for midge-mode.")

(defvar midge-mode-syntax-table (let ((table (make-syntax-table)))
				  (modify-syntax-entry ?\# "<" table)
				  (modify-syntax-entry ?\n ">" table) table)
  "Syntax table for midge-mode.")

(defgroup midge ()
  "Midge generates MIDI files from text input.
Most General MIDI features are supported, and there are some basic methods of
randomly generating sequences.  Also included is a decompiler."
  :group 'external)

(defcustom midge-compiler (executable-find "midge")
  "*Path to the midge executable."
  :group 'midge
  :type 'file)

(defcustom midge-decompiler (executable-find "midi2mg")
  "*Path to the midi2mg executable."
  :group 'midge
  :type 'file)

(defcustom midge-options nil
  "*Options to pass to midge."
  :group 'midge
  :type '(choice (const nil) (list (string :tag "Arg"))))

(defcustom midge-midi-player (or (executable-find "playmidi")
				 (executable-find "timidity"))
  "*Command to play midi files."
  :group 'midge
  :type '(choice (const nil) file))

(defcustom midge-use-menus t
  "*Whether to use the menus."
  :group 'midge
  :type 'boolean)

(defconst midge-comment-regexp "#[^ \n]*"
  "Regular expression for recognizing comments.")

(defconst midge-open-regexp "[^{}#\n]*{"
  "Regular expression for opening bracket.")

(defconst midge-close-regexp "[^{}#\n]*}"
  "Regular expression for closing bracket.")

(defconst midge-close-first-regexp "^[ \t]*}"
  "Regular expression to match a closing bracket as the first
non whitespace char.")

(defconst midge-whitespace-regexp "[ \t]+"
  "Regular expression to match whitespace.")

(defconst midge-blank-line-regexp "^[ \t]*$"
  "Regular expression to match a blank line.")

(defconst midge-tuplet-ratio-regexp "^[0-9]+:[0-9]+$"
  "Regular expression to match time ratio in tuplet blocks.")

(defconst midge-time-sig-regexp "^[0-9]+/[0-9]+$"
  "Regular expression to match time signature.")

(defconst midge-integer-regexp "^[0-9]+$"
  "Regular expression to match an integer")

;; beep and print an error message
(defun midge-error-message (str)
  (ding)
  (message str)
  nil)

;; calculate indent level
(defun midge-indent-level ()
  "Calculate the indent level of the current line."
  (let ((indent 0) (maxpos (line-beginning-position)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) maxpos)
	(cond ((looking-at midge-open-regexp)
	       (goto-char (match-end 0))
	       (setq indent (1+ indent)))
	      ((looking-at midge-close-regexp)
	       (goto-char (match-end 0))
	       (if (> indent 0)
		   (setq indent (1- indent))))
	      ((looking-at midge-comment-regexp)
	       (end-of-line))
	      ((eolp)
	       (if (< (point) maxpos)
		   (progn
		     (next-line 1)
		     (end-of-line)
		     (if (< (point) maxpos)
			 (beginning-of-line)))))
	      (t
	       (end-of-line)))))
    indent))

(defun midge-indent-line ()
  "Indent the current line."
  (interactive)
  (let ((indent (midge-indent-level)) (start-pos (point))
	(current 0) (safe-indent 0))
    (beginning-of-line)
    (if (looking-at midge-close-first-regexp)
	(if (> indent 0)
	    (setq indent (1- indent))))
    (setq safe-indent indent)
    (if (looking-at midge-whitespace-regexp)
	(setq current (- (match-end 0) (point))))
    (if (not (= current (* tab-width indent)))
	(progn
	  (delete-char current 't)
	  (while (> indent 0)
	    (insert "\t")
	    (setq indent (1- indent)))))
    (goto-char start-pos))
  (if (looking-at midge-whitespace-regexp)
      (goto-char (match-end 0))))

(defun midge-close-bracket ()
  "Insert a closing bracket, indenting if neccesary."
  (interactive)
  (let ((start-pos (point)))
    (beginning-of-line)
    (if (looking-at midge-blank-line-regexp)
	(progn
	  (let ((indent 0))
	    (if (looking-at midge-whitespace-regexp)
		(progn
		  (goto-char (match-end 0))
		  (if (>= (point) start-pos)
		      (setq indent 1))))
	    (if (eolp)
		(setq indent 1))
	    (if (= indent 1)
		(progn
		  (beginning-of-line)
		  (if (looking-at midge-whitespace-regexp)
		      (progn
			(kill-line)
			(insert "\n")
			(next-line -1)))
		  (setq indent (1- (midge-indent-level)))
		  (while (> indent 0)
		    (insert "\t")
		    (setq indent (1- indent))))))
	  (insert "}"))
      (goto-char start-pos)
      (insert "}"))))

(defun midge-head-block ()
  "Insert a head block."
  (interactive)
  (midge-indent-line)
  (insert "@head {\n\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -2)
  (midge-indent-line))

(defun midge-body-block ()
  "Insert a body block."
  (interactive)
  (midge-indent-line)
  (insert "@body {\n\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -2)
  (midge-indent-line))

(defun midge-channel-block (number name)
  "Insert a channel block, prompting for channel number
and instrument name, indenting and placing point apropriately."
  (interactive "nChannel number: \nsInstrument name: ")
  (if (or (< number 1) (> number 16))
      (midge-error-message (concat "Bad channel number: "
				   (number-to-string number)))
    (midge-indent-line)
    (if (equal name "")
	(insert (concat "@channel " (number-to-string number) " {\n\n"))
      (insert (concat "@channel " (number-to-string number)
		      " \"" name "\" {\n\n")))
    (midge-close-bracket)
    (insert "\n")
    (next-line -2)
    (midge-indent-line)))

(defun midge-repeat-block (count)
  "Insert a multiline repeat block, indenting as neccesary,
prompting for repeat count and placing point between the
brackets."
  (interactive "nRepeat count: ")
  (midge-indent-line)
  (insert "%repeat ")
  (insert (number-to-string count))
  (insert " {\n")
  (midge-indent-line)
  (insert "\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -1)
  (beginning-of-line)
  (backward-char 1))

(defun midge-choose-block (time)
  "Insert a multiline choose block, indenting as neccesary
and placing point between the brackets."
  (interactive "sRiff length: ")
  (midge-indent-line)
  (insert "%choose ")
  (if (equal time "")
      (insert "{\n")
    (insert (concat time " {\n")))
  (midge-indent-line)
  (insert "\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -1)
  (beginning-of-line)
  (backward-char 1))

(defun midge-chain-block (time)
  "Insert a chain block, indenting as neccesary
and placing point between the brackets."
  (interactive "sRiff length: ")
  (midge-indent-line)
  (insert "%chain ")
  (insert (concat time " {\n"))
  (midge-indent-line)
  (insert "\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -1)
  (beginning-of-line)
  (backward-char 1))

;; This does not work because the current indent function does behave
;; strangely and non-intuitively.
;; (define-skeleton midge-bend-block
;;   ""
;;   "Initial note: "
;;   > "%bend " str " {" \n _ \n "}\n")

(defun midge-bend-block (note)
  "Insert a multiline bend block, indenting as neccesary,
prompting for initial note and placing point between the
brackets."
  (interactive "sInitial note: ")
  (midge-indent-line)
  (insert "%bend ")
  (insert note)
  (insert " {\n")
  (midge-indent-line)
  (insert "\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -1)
  (beginning-of-line)
  (backward-char 1))

(defun midge-define-block (name)
  "Insert a multiline define block, indenting as neccesary,
prompting for riff name and placing point between the
brackets."
  (interactive "sRiff name: ")
  (midge-indent-line)
  (insert "%define ")
  (insert name)
  (insert " {\n")
  (midge-indent-line)
  (insert "\n")
  (midge-close-bracket)
  (insert "\n")
  (next-line -1)
  (beginning-of-line)
  (backward-char 1))

(defun midge-tuplet-block (ratio)
  "Insert a multiline tuplet block, indenting as neccesary,
prompting for time ratio and placing point between the
brackets."
  (interactive "sTime ratio: ")
  (if (not (string-match midge-tuplet-ratio-regexp ratio))
      (midge-error-message
       (concat "bad time ratio `" ratio "\' (should be n:d)"))
    (midge-indent-line)
    (insert "%tuplet ")
    (insert ratio)
    (insert " {\n")
    (midge-indent-line)
    (insert "\n")
    (midge-close-bracket)
    (insert "\n")
    (next-line -1)
    (beginning-of-line)
    (backward-char 1)))

(define-skeleton midge-repeat-line
  "Add a repeat block on a single line, prompting for
number of repeats and positioning point apropriately."
  "Repeat count: "
  "%repeat " str " { " _ " }\n")

(define-skeleton midge-bend-line
  "Add a bend block on a single line, prompting for initial note and
positioning point apropriately."
  "Initial note: "
  "%bend " str " { " _ " }\n")

(define-skeleton midge-define-line
  "Add a define block on a single line, prompting for riff name and
positioning point apropriately."
  "Name of riff: "
  "%define " str " { " _ " }" \n)


(define-skeleton midge-choose-line
  "Add a choose block on a single line, positioning point apropriately."
  "Riff length: "
  "%choose" (unless (string= (eval str) "") (concat " " str)) " { " _ " }\n")

(define-skeleton midge-tuplet-line
  "Add a tuplet block on a single line, prompting for time ratio and
positioning point apropriately."
  "Time ratio: "
  (unless (string-match midge-tuplet-ratio-regexp (eval str))
       (error "bad time ratio %s (should be n:d)" str))
  "%tuplet " str " { " _ " }\n")

(define-skeleton midge-insert-tempo
  "Add a $tempo event, promptimg for the bpm value."
  "Tempo (bpm): "
  "$tempo " str)

(define-skeleton midge-insert-time-sig
  "Add a $time_sig event, promptimg for the time signature."
  "Time signature (n/m): "
  (unless (string-match midge-time-sig-regexp (eval str))
    (error "bad time signature value %s (expected n/m)" str))
  "$time_sig " str)

(defun midge-insert-reverb (value)
  "Add a $reverb event, promptimg for the value."
  (interactive "nReverb (0-127): ")
  (if (and (> 0 value) (< 128 value))
      (midge-error-message (concat "bad reverb value "
				   value
				   " (expected 0-127)"))
    (insert (concat "$reverb " value))))

(defun midge-insert-chorus (value)
  "Add a $chorus event, promptimg for the value."
  (interactive "nChorus (0-127): ")
  (if (and (> 0 value) (< 128 value))
      (midge-error-message (concat "bad chorus value "
				   value
				   " (expected 0-127)"))
    (insert (concat "$chorus " value))))

(defun midge-insert-volume (value)
  "Add a $volume event, promptimg for the value."
  (interactive "nVolume (0-127): ")
  (if (or (> 0 value) (< 127 value))
      (midge-error-message (concat "bad volume value "
				   value
				   " (expected 0-127)"))
    (insert (concat "$volume " value))))

(defun midge-insert-pan (value)
  "Add a $pan event, promptimg for the value."
  (interactive "nPan (0-127): ")
  (if (and (> 0 value) (< 128 value))
      (midge-error-message (concat "bad pan value "
				   value
				   " (expected 0-127)"))
    (insert (concat "$pan " value))))

(defun midge-compile ()
  "Compile the current file."
  (interactive)
  (save-some-buffers)
  (let ((command (concat midge-compiler " " buffer-file-name)))
    (compile-internal command "No more errors")))

(defun midge-compile-background ()
  "Compile the current file."
  (interactive)
  (save-some-buffers)
  (let ((command (concat midge-compiler " " buffer-file-name "&")))
    (compile-internal command "No more errors")))

(defun midge-compile-debug ()
  "Compile the current file with debug option."
  (interactive)
  (save-some-buffers)
  (let ((command (concat midge-compiler " -d " buffer-file-name)))
    (compile-internal command "No more errors")))

(defun midge-compile-verbose ()
  "Compile the current file with verbose option."
  (interactive)
  (save-some-buffers)
  (let ((command (concat midge-compiler " -v " buffer-file-name)))
    (compile-internal command "No more errors")))

(defun midge-compile-ask (options)
  "Compile the current file with verbose option."
  (interactive "sOptions: ")
  (save-some-buffers)
  (let ((command (concat midge-compiler " " options " " buffer-file-name)))
    (compile-internal command "No more errors")))

(defun midge-play-background ()
  "play the midi file generated from the current midge source file."
  (interactive)
  (let ((filename (concat
		   (file-name-sans-extension buffer-file-name) ".mid")))
    (if (not (file-exists-p filename))
	 (midge-error-message "Current file has not been compiled")
      (shell-command (concat midge-midi-player " " filename "&")))))
	
(defun midge-play-foreground ()
  "play the midi file generated from the current midge source file."
  (interactive)
  (let ((filename (concat
		   (file-name-sans-extension buffer-file-name) ".mid")))
    (if (not (file-exists-p filename))
	(midge-error-message "Current file has not been compiled")
      (shell-command (concat midge-midi-player " " filename)))))
	
(defun midge-play-ask-background (file)
  "play the midi file chosen by the user in the background."
  (interactive "fMidi file: ")
  (let ((command (concat midge-midi-player " " file "&")))
    (shell-command command)))

(defun midge-play-ask-foreground (file)
  "play the midi file chosen by the user in the foreground."
  (interactive "fMidi file: ")
  (let ((command (concat midge-midi-player " " file)))
    (shell-command command)))

(defun midge-decompile (file)
  "Decompile the midi file chosen by the user into a new buffer."
  (interactive "fMidi file: ")
  (message (concat "decompiling " file " in background..."))
  (sleep-for 2)
  (let ((command (concat midge-decompiler " -o - " file "&")))
    (shell-command command "*midi2mg output*")))

(defcustom midge-scale-alist
  '(("Major" . "major")
    ("Minor" . "minor")
    ("Modes..."
     ("Ionian (major)" . "ionian")
     ("Dorian" . "dorian")
     ("Phrygian" . "phrygian")
     ("Lydian" . "lydian")
     ("Mixolydian" . "mixolydian")
     ("Aeolian" . "aeolian")
     ("Locrian" . "locrian"))
    ("Jazz/Blues..."
     ("Harmonic Minor" . "minor_harmonic")
     ("Jazz Minor (ascending melodic minor)" . "minor_jazz")
     ("Bebop" . "bebop")
     ("Bebop Dorian" . "bebop_dorian")
     ("Bebop Mixolydian" . "bebop_mixolydian")
     ("Minor Pentatonic" . "minor_pentatonic")
     ("Major Pentatonic" . "major_pentatonic"))
    ("Misc..."
     ("Chromatic (half tone)" . "chromatic")
     ("Whole Tone" . "whole_tone")
     ("Arabian" . "arabian")
     ("Spanish" . "spanish")
     ("Gypsy" . "gypsy")))
  ""
  :group 'midge
  :type `(repeat
	  (choice (cons :tag "Scale group"
			(string :tag "Group name")
			(repeat (cons :format "%v" (string :tag "Verbose name")
				      (choice :tag "Scale name"
					      ,@(mapcar (lambda (s)
							  (list 'const s))
							midge-scale-names)
					      (string :tag "Undefined")))))
		  (cons :tag "Scale mapping"
			(string :tag "Verbose name")
			(string :tag "Scale name")))))

(defun midge-select (alist prompt &optional alt-prompt)
  "Allow the user to choose something interactively."
  (let* ((completion-ignore-case t)
	 (family (cdr (assoc (completing-read prompt alist) alist))))
    (cond
     ((and family (listp family))
      (let ((sym (cdr (assoc (completing-read (or alt-prompt prompt) family)
			     family))))
	(and sym (insert sym))))
     ((stringp family)
      (insert family)))))


(defun midge-select-scale ()
  "Allow the user to choose a scale interactively."
  (interactive)
  (midge-select midge-scale-alist "Select sclae (`TAB' for a list): "))

(defcustom midge-drum-alist
  '(("Bass & Snare Drums"
     ("Acoustic Bass Drum" . "bd_ac")
     ("Bass Drum" . "bd")
     ("Acoustic Snare" . "sd_ac")
     ("Electric Snare" . "sd_el"))
    ("Cymbals"
     ("Closed Hi-Hat" . "hh_c")
     ("Pedal Hi-Hat" . "hh_p")
     ("Open Hi-Hat" . "hh_o")
     ("Crash Cymbal 1" . "cym_crash")
     ("Crash Cymbal 2" . "cym_crash_2")
     ("Ride Cymbal 1" . "cym_ride")
     ("Ride Cymbal 2" . "cym_ride_2")
     ("Chinese Cymbal" . "cym_chinese")
     ("Splash Cymbal" . "cym_splash"))
    ("Toms"
     ("Low Floor Tom" . "ftom_l")
     ("High Floor Tom" . "ftom_h")
     ("Low Tom" . "tom_l")
     ("Low Mid Tom" . "tom_lm")
     ("High Mid Tom" . "tom_hm")
     ("High Tom" . "tom_h"))
    ("Bongos & Congas etc."
     ("High Bongo" . "bongo_h")
     ("Low Bongo" . "bongo_l")
     ("Mute High Conga" . "conga_h_mute")
     ("Open High conga" . "conga_h_open")
     ("Low Conga" . "conga_l")
     ("High Timbale" . "timbale_h")
     ("Low Timbale" . "timbale_l"))
    ("Bells & Whistles etc."
     ("Ride Bell" . "ride_bell")
     ("Cowbell" . "cowbell")
     ("High Agogo" . "agogo_h")
     ("Low Agogo" . "agogo_l")
     ("Muted Triangle" . "tri_mute")
     ("Open Triangle" . "tri_open")
     ("Short Whistle" . "whistle_sh")
     ("Long Whistle" . "whistle_lg"))
    ("Misc"
     ("Side Stick" . "stick")
     ("Hand Clap" . "clap")
     ("Tambourine" . "tamb")
     ("Vibraslap" . "vibraslap")
     ("Cabasa" . "cabasa")
     ("Maracas" . "maracas")
     ("Short Guiro" . "guiro_sh")
     ("Long Guiro" . "guiro_lg")
     ("Claves" . "claves")
     ("High Wood Block" . "wood_h")
     ("Low Wood Block" . "wood_l")
     ("Muted Cuica" . "cuica_mute")
     ("Open Cuica" . "cuica_open")))
  ""
  :group 'midge
  :type `(repeat
	  (choice (cons :tag "Percusion group"
			(string :tag "Group name")
			(repeat (cons :format "%v" (string :tag "Verbose name")
				      (choice :tag "GM name"
					      ,@(mapcar (lambda (s)
							  (list 'const s))
							midge-drum-names)
					      (string :tag "Undefined")))))
		  (cons :tag "Drum mapping"
			(string :tag "Verbose name")
			(string :tag "GM name")))))

(defun midge-select-drum ()
  "Allow the user to choose a drum interactively."
  (interactive)
  (midge-select midge-drum-alist "Select percusion group (`TAB' for a list): "
		"Select instrument: "))

(defcustom midge-patch-alist
  '(("Pianos"
     ("Acoustic Grand Piano"  . "piano_grand_ac")
     ("Bright Acoustic Piano" . "piano_br")
     ("Electric Grand Piano"  . "piano_grand_el")
     ("Honky Tonk Piano"      . "piano_ht")
     ("Electric Piano 1"      . "piano_el_1")
     ("Electric Piano 2"      . "piano_el_2")
     ("Harpsichord"           . "harpsichord")
     ("Clavinet"              . "clavinet"))
    ("Chromatic Percussion"
     ("Celesta"       . "celesta")
     ("Glockenspiel"  . "glockenspiel") 
     ("Music Box"     . "music_box")
     ("Vibraphone"    . "vibraphone")
     ("Marimba"       . "marimba")
     ("Xylophone"     . "xylophone")
     ("Tubular Bells" . "tubular_bells")
     ("Dulcimer"      . "dulcimer"))
    ("Organs"
     ("Drawbar Organ"    . "organ_dbar")
     ("Percussive Organ" . "organ_perc")
     ("Rock Organ"       . "organ_rock")
     ("Church Organ"     . "organ_church")
     ("Reed Organ"       . "organ_reed")
     ("Accoridan"        . "accordian")
     ("Harmonica"        . "harmonica")
     ("Tango Accordian"  . "accordian_tango"))
    ("Guitars"
     ("Nylon String Guitar"   . "guitar_nylon")
     ("Steel String Guitar"   . "guitar_steel")
     ("Electric Jazz Guitar"  . "guitar_jazz")
     ("Electric Clean Guitar" . "guitar_clean")
     ("Electric Muted Guitar" . "guitar_muted")
     ("Overdriven Guitar"     . "guitar_od")
     ("Distortion Guitar"     . "guitar_dist")
     ("Guitar Harmonics"      . "guitar_harm"))
    ("Basses"
     ("Acoustic Bass"          . "bass_ac")
     ("Electric Bass (finger)" . "bass_fg")
     ("Electric Bass (pick)"   . "bass_pick")
     ("Fretless Bass"          . "bass_fless")
     ("Slap Bass 1"            . "bass_slap_1")
     ("Slap Bass 2"            . "bass_slap_2")
     ("Synth Bass 1"           . "bass_syn_1")
     ("Synth Bass 2"           . "bass_syn_2"))
    ("Solo Strings"
     ("Violin"             . "violin")
     ("Viola"              . "viola")
     ("Cello"              . "cello")
     ("Contrabass"         . "contrabass")
     ("Tremolo Strings"    . "str_trem")
     ("Pizzicato Strings"  . "str_pizz")
     ("Orchestral Strings" . "str_orch")
     ("Timpani"            . "timpani"))
    ("String Ensembles"
     ("String Ensemble 1"  . "str_ens_1")
     ("String Ensemble 2"  . "str_ens_2")
     ("SynthStrings 1"     . "str_syn_1")
     ("SynthStrings 2"     . "str_syn_2")
     ("Choir Aahs"         . "choir_aahs")
     ("Voice Oohs"         . "voice_oohs")
     ("Synth Voice"        . "voice_syn")
     ("Orchestra Hit"      . "orch_hit"))
    ("Brass"
     ("Trumpet"            . "trumpet")
     ("Trombone"           . "trombone")
     ("Tuba"               . "tuba")
     ("Muted Trumpet"      . "trumpet_muted")
     ("French Horn"        . "horn_fr")
     ("Brass Section"      . "brass")
     ("SynthBrass 1"       . "brass_syn_1")
     ("SynthBrass 2"       . "brass_syn_2"))
    ("Reed"
     ("Soprano Sax"        . "sax_sop")
     ("Alto Sax"           . "sax_alt")
     ("Tenor Sax"          . "sax_ten")
     ("Baritone Sax"       . "sax_bar")
     ("Oboe"               . "oboe")
     ("English Horn"       . "horn_en")
     ("Bassoon"            . "bassoon")
     ("Clarinet"           . "clarinet"))
    ("Pipe"
     ("Piccolo"            . "piccolo")
     ("Flute"              . "flute")
     ("Recorder"           . "recorder")
     ("Pan Flute"          . "flute_pan")
     ("Blown Bottle"       . "bottle")
     ("Skakuhachi"         . "skakuhachi")
     ("Whistle"            . "whistle")
     ("Ocarina"            . "ocarina"))
    ("Synth Leads"
     ("Lead 1 (square)"    . "lead_sq")
     ("Lead 2 (sawtooth)"  . "lead_saw")
     ("Lead 3 (calliope)"  . "lead_calliope")
     ("Lead 4 (chiff)"     . "lead_chiff")
     ("Lead 5 (charang)"   . "lead_charang")
     ("Lead 6 (voice)"     . "lead_voice")
     ("Lead 7 (fifths)"    . "lead_fifth")
     ("Lead 8 (bass+lead)" . "lead_basslead"))
    ("Synth Pads"
     ("Pad 1 (new age)"    . "pad_new_age")
     ("Pad 2 (warm)"       . "pad_warm")
     ("Pad 3 (polysynth)"  . "polysynth")
     ("Pad 4 (choir)"      . "pad_choir")
     ("Pad 5 (bowed)"      . "pad_bowed")
     ("Pad 6 (metallic)"   . "pad_metal")
     ("Pad 7 (halo)"       . "pad_halo")
     ("Pad 8 (sweep)"      . "pad_sweep"))
    ("Synth Effects"
     ("FX 1 (rain)"        . "fx_rain")
     ("FX 2 (soundtrack)"  . "fx_strack")
     ("FX 3 (crystal)"     . "fx_crystal")
     ("FX 4 (atmosphere)"  . "fx_atmos")
     ("FX 5 (brightness)"  . "fx_bright")
     ("FX 6 (goblins)"     . "fx_goblin")
     ("FX 7 (echoes)"      . "fx_echo")
     ("FX 8 (sci-fi)"      . "fx_scifi"))
    ("Ethnic"
     ("Sitar" . "sitar")
     ("Banjo" . "banjo")
     ("Shamisen" . "shamisen")
     ("Koto" . "koto")
     ("Kalimba" . "kalimba")
     ("Bagpipe" . "bagpipe")
     ("Fiddle" . "fiddle")
     ("Shanai" . "shanai"))
    ("Percussive"
     ("Tinkle Bell" . "bell_tinkle")
     ("Agogo" . "agogo")
     ("Steel Drums" . "drum_steel")
     ("Woodblock" . "woodblock")
     ("Taiko Drum" . "drum_taiko")
     ("Melodic Tom" . "tom_melodic")
     ("Synth Drum" . "drum_syn")
     ("Reverse Cymbal" . "cymbal_rev"))
    ("Sound Effects"
     ("Guitar Fret Noise" . "fx_fret")
     ("Breath Noise" . "fx_breath")
     ("Seashore" . "fx_sea")
     ("Bird Tweet" . "fx_tweet")
     ("Telephone Ring" . "fx_phone")
     ("Helicopter" . "fx_copter")
     ("Applause" . "fx_applause")
     ("Gunshot" . "fx_gun")))
  "A mapping from verbose instrument names to GM patch names."
  :group 'midge
  :type `(repeat
	  (choice (cons :tag "Instrument group"
			(string :tag "Group name")
			(repeat (cons :format "%v" (string :tag "Verbose name")
				      (choice :tag "GM name"
					      ,@(mapcar (lambda (s)
							  (list 'const s))
							midge-instrument-names)
					      (string :tag "Undefined")))))
		  (cons :tag "Instrument mapping"
			(string :tag "Verbose name")
			(string :tag "GM name")))))

(defun midge-select-patch ()
  "Allow the user to choose a patch interactively."
  (interactive)
  (midge-select midge-patch-alist "Choose patch family (`TAB' for a list): "
		"Instrument: "))

(defvar midge-menu
  (list "Midge"
	(list "Block..."
	      ["head"      midge-head-block t]
	      ["body"      midge-body-block t]
	      ["channel"   midge-channel-block t]
	      "-"
	      ["define"    midge-define-block t]
	      ["repeat"    midge-repeat-block t]
	      ["bend"      midge-bend-block t]
	      ["choose"    midge-choose-block t]
	      ["chain"     midge-chain-block t]
	      ["tuplet"    midge-tuplet-block t]
	      )
	(list "Line..."
	      ["repeat"    midge-repeat-line t]
	      ["bend"      midge-bend-line t]
	      ["choose"    midge-choose-line t]
	      ["chain"     midge-chain-line t]
	      ["tuplet"    midge-tuplet-line t]
	      )
	(list "value..."
	      ["tempo"     midge-insert-tempo t]
	      ["time sig"  midge-insert-time-sig t]
	      ["volume"    midge-insert-volume t]
	      ["pan"       midge-insert-pan t]
	      ["reverb"    midge-insert-reverb t]
	      ["chorus"    midge-insert-chorus t]
	      )
	"-"
	["Drum"    midge-select-drum t]
	["Patch"   midge-select-patch t]
	["Scale"   midge-select-scale t]
	"-"
	(list "Compile..."
	      ["Simple"          midge-compile t]
	      ["Verbose"         midge-compile-verbose t]
	      ["Debug"           midge-compile-debug t]
	      ["Prompt for args" midge-compile-ask t]
	      )
	(list "Play..."
	      ["Current foreground" midge-play-foreground t]
	      ["Current background" midge-play-background t]
	      ["Prompt foreground"  midge-play-ask-foreground t]
	      ["Prompt background"  midge-play-ask-background t]
	      )
	["Decompile"           midge-decompile t]))

(when midge-use-menus
  (require 'easymenu)
  (easy-menu-do-define 'midge-menu1
		       midge-mode-map
		       "Menu for midge-mode."
		       midge-menu))

(defun midge-mode ()
  "Midge mode, for writing midge(1) source files.

\\{midge-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table midge-mode-syntax-table)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'indent-line-function) #'midge-indent-line)
  (set (make-local-variable 'font-lock-defaults) '(midge-font-lock-keywords))
  (setq major-mode 'midge-mode)
  (setq mode-name "Midge")
  (use-local-map midge-mode-map)
  (setq tab-width 4)
  (run-hooks 'midge-mode-hook))

(provide 'midge-mode)

;;; midge-mode.el ends here
