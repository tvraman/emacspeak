;;;Auto generated

;;;### (autoloads (cd-tool) "cd-tool" "cd-tool.el" (16045 22315))
;;; Generated autoloads from cd-tool.el

(autoload (quote cd-tool) "cd-tool" "\
Front-end to CDTool.
Bind this function to a convenient key-
Emacspeak users automatically have 
this bound to <DEL> in the emacspeak keymap.

Key     Action
---     ------

+       Next Track
-       Previous Track
SPC     Pause or Resume
e       Eject
=       Shuffle
i       CD Info
p       Play
s       Stop
t       track
c       clip
cap C   Save clip to disk
" t nil)

;;;***

;;;### (autoloads (dtk-say dtk-speak-list dtk-speak tts-restart)
;;;;;;  "dtk-speak" "dtk-speak.el" (16045 22321))
;;; Generated autoloads from dtk-speak.el

(defvar dtk-punctuation-mode "all" "\
Current setting of punctuation state.
Possible values are some, all or none.
You should not modify this variable;
Use command  `dtk-set-punctuations' bound to
\\[dtk-set-punctuations].  .")

(autoload (quote tts-restart) "dtk-speak" "\
Use this to nuke the currently running TTS server and restart it." t nil)

(autoload (quote dtk-speak) "dtk-speak" "\
Speak the TEXT string on the  tts.
This is achieved by sending the text to the speech server.
No-op if variable `dtk-quiet' is set to nil.
If option `outline-minor-mode' is on and selective display is in effect,
only speak upto the first ctrl-m." nil nil)

(autoload (quote dtk-speak-list) "dtk-speak" "\
Speak a  list of strings.
Argument TEXT  is the list of strings to speak." nil nil)

(defsubst dtk-letter (letter) "\
Speak a LETTER." (declare (special dtk-speaker-process dtk-speak-server-initialized dtk-quiet)) (unless dtk-quiet (when dtk-speak-server-initialized (dtk-interp-letter letter))))

(autoload (quote dtk-say) "dtk-speak" "\
Say these WORDS." nil nil)

;;;***

;;;### (autoloads (tts-eflite) "eflite-voices" "eflite-voices.el"
;;;;;;  (16045 22326))
;;; Generated autoloads from eflite-voices.el

(autoload (quote tts-eflite) "eflite-voices" "\
Use eflite TTS server." t nil)

;;;***

;;;### (autoloads (emacspeak-setup-programming-mode) "emacspeak"
;;;;;;  "emacspeak.el" (16045 22350))
;;; Generated autoloads from emacspeak.el

(autoload (quote emacspeak-setup-programming-mode) "emacspeak" "\
Setup programming mode. Turns on audio indentation and
sets punctuation mode to all, activates the dictionary and turns on split caps." nil nil)

;;;***

;;;### (autoloads (emacspeak-aumix-volume-decrease emacspeak-aumix-volume-increase
;;;;;;  emacspeak-aumix-wave-decrease emacspeak-aumix-wave-increase
;;;;;;  emacspeak-aumix-reset emacspeak-aumix-reset-options emacspeak-aumix-settings-file)
;;;;;;  "emacspeak-aumix" "emacspeak-aumix.el" (16045 22338))
;;; Generated autoloads from emacspeak-aumix.el

(defgroup emacspeak-aumix nil "Customization group for setting the Emacspeak auditory\ndisplay." :group (quote emacspeak))

(defvar emacspeak-aumix-settings-file (when (file-exists-p (expand-file-name ".aumixrc" emacspeak-resource-directory)) (expand-file-name ".aumixrc" emacspeak-resource-directory)) "\
*Name of file containing personal aumix settings.")

(defvar emacspeak-aumix-reset-options (format "-f %s -L 2>&1 >/dev/null" emacspeak-aumix-settings-file) "\
*Option to pass to aumix for resetting to default values.")

(autoload (quote emacspeak-aumix-reset) "emacspeak-aumix" "\
Reset to default audio settings." t nil)

(autoload (quote emacspeak-aumix-wave-increase) "emacspeak-aumix" "\
Increase volume of wave output. " t nil)

(autoload (quote emacspeak-aumix-wave-decrease) "emacspeak-aumix" "\
Decrease volume of wave output. " t nil)

(autoload (quote emacspeak-aumix-volume-increase) "emacspeak-aumix" "\
Increase overall volume. " t nil)

(autoload (quote emacspeak-aumix-volume-decrease) "emacspeak-aumix" "\
Decrease overall volume. " t nil)

;;;***

;;;### (autoloads (emacspeak-custom-goto-toolbar emacspeak-custom-goto-group)
;;;;;;  "emacspeak-custom" "emacspeak-custom.el" (16045 22347))
;;; Generated autoloads from emacspeak-custom.el

(autoload (quote emacspeak-custom-goto-group) "emacspeak-custom" "\
Jump to custom group when in a customization buffer." t nil)

(autoload (quote emacspeak-custom-goto-toolbar) "emacspeak-custom" "\
Jump to custom toolbar when in a customization buffer." t nil)

;;;***

;;;### (autoloads (emacspeak-daisy-open-book) "emacspeak-daisy" "emacspeak-daisy.el"
;;;;;;  (16045 22348))
;;; Generated autoloads from emacspeak-daisy.el

(defgroup emacspeak-daisy nil "Daisy Digital Talking Books  for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-daisy-open-book) "emacspeak-daisy" "\
Open Digital Talking Book specified by navigation file filename." t nil)

;;;***

;;;### (autoloads (emacspeak-eterm-remote-term emacspeak-eterm-cache-remote-host
;;;;;;  emacspeak-eterm-record-window) "emacspeak-eterm" "emacspeak-eterm.el"
;;;;;;  (16045 22352))
;;; Generated autoloads from emacspeak-eterm.el

(defgroup emacspeak-eterm nil "Terminal emulator for the Emacspeak Desktop." :group (quote emacspeak) :prefix "emacspeak-eterm-")

(autoload (quote emacspeak-eterm-record-window) "emacspeak-eterm" "\
Insert this window definition into the table of terminal windows.
Argument WINDOW-ID specifies the window.
Argument TOP-LEFT  specifies top-left of window.
Argument BOTTOM-RIGHT  specifies bottom right of window.
Optional argument RIGHT-STRETCH  specifies if the window stretches to the right.
Optional argument LEFT-STRETCH  specifies if the window stretches to the left." nil nil)

(autoload (quote emacspeak-eterm-cache-remote-host) "emacspeak-eterm" "\
Add this hostname to cache of remote hostnames" nil nil)

(autoload (quote emacspeak-eterm-remote-term) "emacspeak-eterm" "\
Start a terminal-emulator in a new buffer." t nil)

;;;***

;;;### (autoloads (emacspeak-filtertext) "emacspeak-filtertext" "emacspeak-filtertext.el"
;;;;;;  (16045 22357))
;;; Generated autoloads from emacspeak-filtertext.el

(autoload (quote emacspeak-filtertext) "emacspeak-filtertext" "\
Copy over text in region to special filtertext buffer in
preparation for interactively filtering text. " t nil)

;;;***

;;;### (autoloads (emacspeak-fix-interactive) "emacspeak-fix-interactive"
;;;;;;  "emacspeak-fix-interactive.el" (16045 22359))
;;; Generated autoloads from emacspeak-fix-interactive.el

(defsubst emacspeak-should-i-fix-interactive-p (sym) "\
Predicate to test if this function should be fixed. " (declare (special emacspeak-commands-dont-fix-regexp)) (and (commandp sym) (not (get sym (quote emacspeak-fixed))) (stringp (second (ad-interactive-form (symbol-function sym)))) (not (string-match emacspeak-commands-dont-fix-regexp (symbol-name sym)))))

(autoload (quote emacspeak-fix-interactive) "emacspeak-fix-interactive" "\
Auto-advice interactive command to speak its prompt.  
Fix the function definition of sym to make its interactive form
speak its prompts. " nil nil)

(defsubst emacspeak-fix-interactive-command-if-necessary (command) "\
Fix command if necessary." (and (emacspeak-should-i-fix-interactive-p command) (emacspeak-fix-interactive command)))

;;;***

;;;### (autoloads (emacspeak-forms-find-file) "emacspeak-forms" "emacspeak-forms.el"
;;;;;;  (16045 22360))
;;; Generated autoloads from emacspeak-forms.el

(autoload (quote emacspeak-forms-find-file) "emacspeak-forms" "\
Visit a forms file" t nil)

;;;***

;;;### (autoloads (emacspeak-freeamp emacspeak-freeamp-freeamp-call-command
;;;;;;  emacspeak-freeamp-freeamp-command) "emacspeak-freeamp" "emacspeak-freeamp.el"
;;;;;;  (16045 22360))
;;; Generated autoloads from emacspeak-freeamp.el

(autoload (quote emacspeak-freeamp-freeamp-command) "emacspeak-freeamp" "\
Execute FreeAmp command." t nil)

(autoload (quote emacspeak-freeamp-freeamp-call-command) "emacspeak-freeamp" "\
Call appropriate freeamp command." t nil)

(autoload (quote emacspeak-freeamp) "emacspeak-freeamp" "\
Play specified resource using freeamp.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-freeamp-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-gridtext-apply emacspeak-gridtext-save
;;;;;;  emacspeak-gridtext-load) "emacspeak-gridtext" "emacspeak-gridtext.el"
;;;;;;  (16045 22364))
;;; Generated autoloads from emacspeak-gridtext.el

(autoload (quote emacspeak-gridtext-load) "emacspeak-gridtext" "\
Load saved grid settings." t nil)

(autoload (quote emacspeak-gridtext-save) "emacspeak-gridtext" "\
Save out grid settings." t nil)

(autoload (quote emacspeak-gridtext-apply) "emacspeak-gridtext" "\
Apply grid to region." t nil)

;;;***

;;;### (autoloads (emacspeak-hide-speak-block-sans-prefix emacspeak-hide-or-expose-all-blocks
;;;;;;  emacspeak-hide-or-expose-block) "emacspeak-hide" "emacspeak-hide.el"
;;;;;;  (16045 22364))
;;; Generated autoloads from emacspeak-hide.el

(autoload (quote emacspeak-hide-or-expose-block) "emacspeak-hide" "\
Hide or expose a block of text.
This command either hides or exposes a block of text
starting on the current line.  A block of text is defined as
a portion of the buffer in which all lines start with a
common PREFIX.  Optional interactive prefix arg causes all
blocks in current buffer to be hidden or exposed." t nil)

(autoload (quote emacspeak-hide-or-expose-all-blocks) "emacspeak-hide" "\
Hide or expose all blocks in buffer." t nil)

(autoload (quote emacspeak-hide-speak-block-sans-prefix) "emacspeak-hide" "\
Speaks current block after stripping its prefix.
If the current block is not hidden, it first hides it.
This is useful because as you locate blocks, you can invoke this
command to listen to the block,
and when you have heard enough navigate easily  to move past the block." t nil)

;;;***

;;;### (autoloads (emacspeak-imcom) "emacspeak-imcom" "emacspeak-imcom.el"
;;;;;;  (16045 22366))
;;; Generated autoloads from emacspeak-imcom.el

(defgroup emacspeak-imcom nil "Jabber access from the Emacspeak audio desktop.")

(autoload (quote emacspeak-imcom) "emacspeak-imcom" "\
Start IMCom." t nil)

;;;***

;;;### (autoloads (emacspeak-keymap-remove-emacspeak-edit-commands
;;;;;;  emacspeak-keymap-choose-new-emacspeak-prefix) "emacspeak-keymap"
;;;;;;  "emacspeak-keymap.el" (16045 22367))
;;; Generated autoloads from emacspeak-keymap.el

(defvar emacspeak-keymap nil "\
Primary keymap used by emacspeak. ")

(autoload (quote emacspeak-keymap-choose-new-emacspeak-prefix) "emacspeak-keymap" "\
Interactively select a new prefix key to use for all emacspeak
commands.  The default is to use `C-e'  This command
lets you switch the prefix to something else.  This is a useful thing
to do if you run emacspeak on a remote machine from inside a terminal
that is running inside a local emacspeak session.  You can have the
remote emacspeak use a different control key to give your fingers some
relief." t nil)

(autoload (quote emacspeak-keymap-remove-emacspeak-edit-commands) "emacspeak-keymap" "\
We define keys that invoke editting commands to be
undefined" nil nil)

;;;***

;;;### (autoloads (emacspeak-m-player) "emacspeak-m-player" "emacspeak-m-player.el"
;;;;;;  (16045 22371))
;;; Generated autoloads from emacspeak-m-player.el

(defgroup emacspeak-m-player nil "Emacspeak media player settings.")

(autoload (quote emacspeak-m-player) "emacspeak-m-player" "\
Play specified resource using m-player.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-m-player-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-ocr) "emacspeak-ocr" "emacspeak-ocr.el"
;;;;;;  (16045 22372))
;;; Generated autoloads from emacspeak-ocr.el

(defgroup emacspeak-ocr nil "Emacspeak front end for scanning and OCR.\nPre-requisites:\nSANE for image acquisition.\nOCR engine for optical character recognition." :group (quote emacspeak) :prefix "emacspeak-ocr-")

(autoload (quote emacspeak-ocr) "emacspeak-ocr" "\
An OCR front-end for the Emacspeak desktop.  

Page image is acquired using tools from the SANE package.
The acquired image is run through the OCR engine if one is
available, and the results placed in a buffer that is
suitable for browsing the results.

For detailed help, invoke command emacspeak-ocr bound to
\\[emacspeak-ocr] to launch emacspeak-ocr-mode, and press
`?' to display mode-specific help for emacspeak-ocr-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-personality-prepend emacspeak-personality-append
;;;;;;  emacspeak-personality-put) "emacspeak-personality" "emacspeak-personality.el"
;;;;;;  (16045 22373))
;;; Generated autoloads from emacspeak-personality.el

(autoload (quote emacspeak-personality-put) "emacspeak-personality" "\
Apply personality to specified region, over-writing any current
personality settings." nil nil)

(autoload (quote emacspeak-personality-append) "emacspeak-personality" "\
Append specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved." nil nil)

(autoload (quote emacspeak-personality-prepend) "emacspeak-personality" "\
Prepend specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved." nil nil)

;;;***

;;;### (autoloads (emacspeak-pronounce-augment-pronunciations emacspeak-pronounce-refresh-pronunciations
;;;;;;  emacspeak-pronounce-toggle-use-of-dictionaries emacspeak-pronounce-clear-dictionaries
;;;;;;  emacspeak-pronounce-load-dictionaries emacspeak-pronounce-save-dictionaries
;;;;;;  emacspeak-pronounce-add-super emacspeak-pronounce-add-dictionary-entry)
;;;;;;  "emacspeak-pronounce" "emacspeak-pronounce.el" (16045 22374))
;;; Generated autoloads from emacspeak-pronounce.el

(autoload (quote emacspeak-pronounce-add-dictionary-entry) "emacspeak-pronounce" "\
Add dictionary entry.
This adds pronunciation pair
STRING.PRONUNCIATION to the   dictionary.
Argument KEY specifies a dictionary key e.g. directory, mode etc." nil nil)

(autoload (quote emacspeak-pronounce-add-super) "emacspeak-pronounce" "\
Make CHILD inherit PARENT's pronunciations." nil nil)

(defsubst emacspeak-pronounce-apply-pronunciations (pronunciation-table) "\
Applies pronunciations specified in pronunciation table to current buffer.
Modifies text and point in buffer." (declare (special emacspeak-pronounce-pronunciation-personality)) (loop for key being the hash-keys of pronunciation-table do (let ((word (symbol-name key)) (pronunciation (gethash key pronunciation-table)) (personality nil)) (goto-char (point-min)) (while (search-forward word nil t) (setq personality (get-text-property (point) (quote personality))) (replace-match pronunciation t t) (if emacspeak-pronounce-pronunciation-personality (put-text-property (match-beginning 0) (+ (match-beginning 0) (length pronunciation)) (quote personality) emacspeak-pronounce-pronunciation-personality) (put-text-property (match-beginning 0) (+ (match-beginning 0) (length pronunciation)) (quote personality) personality))))))

(autoload (quote emacspeak-pronounce-save-dictionaries) "emacspeak-pronounce" "\
Writes out the persistent emacspeak pronunciation dictionaries." t nil)

(defvar emacspeak-pronounce-dictionaries-loaded nil "\
Indicates if dictionaries already loaded.")

(autoload (quote emacspeak-pronounce-load-dictionaries) "emacspeak-pronounce" "\
Load pronunciation dictionaries.
Optional argument FILENAME specifies the dictionary file." t nil)

(autoload (quote emacspeak-pronounce-clear-dictionaries) "emacspeak-pronounce" "\
Clear all current pronunciation dictionaries." t nil)

(defvar emacspeak-pronounce-pronunciation-table nil "\
Variable holding association list of pronunciations for a buffer.
Becomes automatically buffer local.")

(autoload (quote emacspeak-pronounce-toggle-use-of-dictionaries) "emacspeak-pronounce" "\
Toggle use of pronunciation dictionaries in current buffer.
Pronunciations can be dfined on a per file, per directory and/or per
mode basis.
Pronunciations are activated on a per buffer basis.
Turning on the use of pronunciation dictionaries results in emacspeak
composing a pronunciation table based on the currently defined
pronunciation dictionaries.
After this, the pronunciations will be applied whenever text in the
buffer is spoken.
Optional argument state can be used from Lisp programs to
explicitly turn pronunciations on or off." t nil)

(autoload (quote emacspeak-pronounce-refresh-pronunciations) "emacspeak-pronounce" "\
Refresh pronunciation table for current buffer.
Activates pronunciation dictionaries if not already active." t nil)

(autoload (quote emacspeak-pronounce-augment-pronunciations) "emacspeak-pronounce" "\
Pushes pronunciations in specified dictionary on to the dictionary
for the specified mode." nil nil)

;;;***

;;;### (autoloads (emacspeak-realaudio-browse emacspeak-realaudio
;;;;;;  emacspeak-realaudio-process-sentinel emacspeak-realaudio-play)
;;;;;;  "emacspeak-realaudio" "emacspeak-realaudio.el" (16045 22375))
;;; Generated autoloads from emacspeak-realaudio.el

(autoload (quote emacspeak-realaudio-play) "emacspeak-realaudio" "\
Play a realaudio stream.  Uses files from your Realaudio
shortcuts directory for completion.  See documentation for
user configurable variable
emacspeak-realaudio-shortcuts-directory. " t nil)

(autoload (quote emacspeak-realaudio-process-sentinel) "emacspeak-realaudio" "\
Cleanup after realaudio is done. " nil nil)

(autoload (quote emacspeak-realaudio) "emacspeak-realaudio" "\
Start or control streaming audio including MP3 and
realaudio.  If using `TRPlayer' as the player, accepts
trplayer control commands if a stream is already playing.
Otherwise, the playing stream is simply stopped.  If no
stream is playing, this command prompts for a realaudio
resource.  Realaudio resources can be specified either as a
Realaudio URL, the location of a local Realaudio file, or as
the name of a local Realaudio metafile. Realaudio resources
you have played in this session are available in the
minibuffer history.  The default is to play the resource you
played most recently. Emacspeak uses the contents of the
directory specified by variable
emacspeak-realaudio-shortcuts-directory to offer a set of
completions. Hit space to use this completion list.

If using TRPlayer, you can either give one-shot commands
using command emacspeak-realaudio available from anywhere on
the audio desktop as `\\[emacspeak-realaudio]'.
Alternatively, switch to buffer *realaudo* using
`\\[emacspeak-realaudio];' if you wish to issue many
navigation commands.  Note that buffer *realaudio* uses a
special major mode that provides the various navigation
commands via single keystrokes." t nil)

(autoload (quote emacspeak-realaudio-browse) "emacspeak-realaudio" "\
Browse RAM file before playing the selected component." t nil)

;;;***

;;;### (autoloads nil "emacspeak-redefine" "emacspeak-redefine.el"
;;;;;;  (16045 22375))
;;; Generated autoloads from emacspeak-redefine.el

(when (subrp (symbol-function (quote ding))) (fset (quote orig-ding) (symbol-function (quote ding))) (defun ding (&optional arg) "Beep, or flash the screen.\nAlso, unless an argument is given,\nterminate any keyboard macro currently executing.\nAdditionally, emacspeak sets this up to play an auditory icon. " (emacspeak-auditory-icon (quote warn-user)) (orig-ding arg)))

;;;***

;;;### (autoloads (emacspeak-remote-connect-to-server emacspeak-remote-ssh-to-server
;;;;;;  emacspeak-remote-quick-connect-to-server) "emacspeak-remote"
;;;;;;  "emacspeak-remote.el" (16045 22377))
;;; Generated autoloads from emacspeak-remote.el

(defgroup emacspeak-remote nil "Emacspeak remote group." :group (quote emacspeak-remote))

(autoload (quote emacspeak-remote-quick-connect-to-server) "emacspeak-remote" "\
Connect to remote server.
Does not prompt for host or port, but quietly uses the
guesses that appear as defaults when prompting.
Use this once you are sure the guesses are usually correct." t nil)

(autoload (quote emacspeak-remote-ssh-to-server) "emacspeak-remote" "\
Open ssh session to where we came from." t nil)

(autoload (quote emacspeak-remote-connect-to-server) "emacspeak-remote" "\
Connect to and start using remote speech server running on host host
and listening on port port.  Host is the hostname of the remote
server, typically the desktop machine.  Port is the tcp port that that
host is listening on for speech requests." t nil)

;;;***

;;;### (autoloads (emacspeak-rss-browse emacspeak-rss-display) "emacspeak-rss"
;;;;;;  "emacspeak-rss.el" (16045 22380))
;;; Generated autoloads from emacspeak-rss.el

(defgroup emacspeak-rss nil "RSS Feeds for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-rss-display) "emacspeak-rss" "\
Retrieve and display RSS news feed." t nil)

(autoload (quote emacspeak-rss-browse) "emacspeak-rss" "\
Browse specified RSS feed." t nil)

;;;***

;;;### (autoloads nil "emacspeak-setup" "emacspeak-setup.el" (16045
;;;;;;  22380))
;;; Generated autoloads from emacspeak-setup.el

(defvar emacspeak-directory (expand-file-name "../" (file-name-directory load-file-name)) "\
Directory where emacspeak is installed. ")

(defvar emacspeak-lisp-directory (expand-file-name "lisp/" emacspeak-directory) "\
Directory where emacspeak lisp files are  installed. ")

(defvar emacspeak-sounds-directory (expand-file-name "sounds/" emacspeak-directory) "\
Directory containing auditory icons for Emacspeak.")

(defvar emacspeak-etc-directory (expand-file-name "etc/" emacspeak-directory) "\
Directory containing miscellaneous files  for
  Emacspeak.")

(defvar emacspeak-servers-directory (expand-file-name "servers/" emacspeak-directory) "\
Directory containing speech servers  for
  Emacspeak.")

(defvar emacspeak-info-directory (expand-file-name "info/" emacspeak-directory) "\
Directory containing  Emacspeak info files.")

(defvar emacspeak-resource-directory (expand-file-name "~/.emacspeak/") "\
Directory where Emacspeak resource files such as
pronunciation dictionaries are stored. ")

;;;***

;;;### (autoloads (emacspeak-set-auditory-icon-player emacspeak-toggle-auditory-icons
;;;;;;  emacspeak-sounds-select-theme emacspeak-play-program emacspeak-sounds-default-theme
;;;;;;  emacspeak-sounds-define-theme) "emacspeak-sounds" "emacspeak-sounds.el"
;;;;;;  (16045 22380))
;;; Generated autoloads from emacspeak-sounds.el

(defsubst emacspeak-using-midi-p nil "\
Predicate to test if we are using midi." (declare (special emacspeak-auditory-icon-function)) (or (eq emacspeak-auditory-icon-function (quote emacspeak-play-midi-icon)) (eq emacspeak-auditory-icon-function (quote emacspeak-queue-midi-icon))))

(autoload (quote emacspeak-sounds-define-theme) "emacspeak-sounds" "\
Define a sounds theme for auditory icons. " nil nil)

(defvar emacspeak-sounds-default-theme (expand-file-name "default-8k/" emacspeak-sounds-directory) "\
Default theme for auditory icons. ")

(defvar emacspeak-play-program (cond ((getenv "EMACSPEAK_PLAY_PROGRAM")) ((file-exists-p "/usr/bin/play") "/usr/bin/play") ((file-exists-p "/usr/bin/audioplay") "/usr/bin/audioplay") ((file-exists-p "/usr/demo/SOUND/play") "/usr/demo/SOUND/play") (t (expand-file-name emacspeak-etc-directory "play"))) "\
Name of executable that plays sound files. ")

(autoload (quote emacspeak-sounds-select-theme) "emacspeak-sounds" "\
Select theme for auditory icons." t nil)

(defsubst emacspeak-get-sound-filename (sound-name) "\
Retrieve name of sound file that produces  auditory icon SOUND-NAME." (declare (special emacspeak-sounds-themes-table emacspeak-sounds-current-theme)) (let ((f (expand-file-name (format "%s%s" sound-name (emacspeak-sounds-theme-get-extension emacspeak-sounds-current-theme)) emacspeak-sounds-current-theme))) (if (file-exists-p f) f emacspeak-default-sound)))

(defsubst emacspeak-queue-auditory-icon (sound-name) "\
Queue auditory icon SOUND-NAME." (declare (special dtk-speaker-process)) (process-send-string dtk-speaker-process (format "a %s
" (emacspeak-get-sound-filename sound-name))))

(defsubst emacspeak-native-auditory-icon (sound-name) "\
Play auditory icon using native Emacs player." (play-sound (list (quote sound) :file (format "%s" (emacspeak-get-sound-filename sound-name)))))

(defsubst emacspeak-serve-auditory-icon (sound-name) "\
Serve auditory icon SOUND-NAME.
Sound is served only if `emacspeak-use-auditory-icons' is true.
See command `emacspeak-toggle-auditory-icons' bound to \\[emacspeak-toggle-auditory-icons ]." (declare (special dtk-speaker-process emacspeak-use-auditory-icons)) (when emacspeak-use-auditory-icons (process-send-string dtk-speaker-process (format "p %s
" (emacspeak-get-sound-filename sound-name)))))

(defsubst emacspeak-play-midi-icon (midi-name) "\
Play midi icon midi-NAME." (apply (quote dtk-force-note) (emacspeak-get-midi-note midi-name)))

(defsubst emacspeak-auditory-icon (icon) "\
Play an auditory ICON." (declare (special emacspeak-auditory-icon-function)) (funcall emacspeak-auditory-icon-function icon))

(autoload (quote emacspeak-toggle-auditory-icons) "emacspeak-sounds" "\
Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value." t nil)

(autoload (quote emacspeak-set-auditory-icon-player) "emacspeak-sounds" "\
Select  player used for producing auditory icons.
Recommended choices:

emacspeak-serve-auditory-icon for  the wave device.
emacspeak-play-midi-icon for midi device. " t nil)

;;;***

;;;### (autoloads (emacspeak-toggle-comint-output-monitor emacspeak-speak-world-clock)
;;;;;;  "emacspeak-speak" "emacspeak-speak.el" (16045 22381))
;;; Generated autoloads from emacspeak-speak.el

(autoload (quote emacspeak-speak-world-clock) "emacspeak-speak" "\
Display current date and time  for specified zone." t nil)

(autoload (quote emacspeak-toggle-comint-output-monitor) "emacspeak-speak" "\
Toggle state of Emacspeak comint monitor.
When turned on, comint output is automatically spoken.  Turn this on if
you want your shell to speak its results.  Interactive
PREFIX arg means toggle the global default value, and then
set the current local value to the result." t nil)

;;;***

;;;### (autoloads (emacspeak-table-make-table) "emacspeak-table"
;;;;;;  "emacspeak-table.el" (16045 22383))
;;; Generated autoloads from emacspeak-table.el

(autoload (quote emacspeak-table-make-table) "emacspeak-table" "\
Construct a table object from elements." nil nil)

;;;***

;;;### (autoloads (emacspeak-table-copy-to-clipboard emacspeak-table-display-table-in-region
;;;;;;  emacspeak-table-view-csv-buffer emacspeak-table-find-csv-file
;;;;;;  emacspeak-table-find-file) "emacspeak-table-ui" "emacspeak-table-ui.el"
;;;;;;  (16045 22385))
;;; Generated autoloads from emacspeak-table-ui.el

(defsubst emacspeak-table-prepare-table-buffer (table buffer &optional filename) "\
Prepare tabular data." (declare (special positions)) (save-excursion (set-buffer buffer) (let ((i 0) (j 0) (count 0) (row-start 1) (column-start 1) (inhibit-read-only t)) (setq truncate-lines t) (erase-buffer) (set (make-local-variable (quote emacspeak-table)) table) (set (make-local-variable (quote positions)) (make-hash-table)) (when filename (setq buffer-file-name filename)) (setq count (1- (emacspeak-table-num-columns table))) (loop for row across (emacspeak-table-elements table) do (loop for element across row do (setf (gethash (intern (format "element:%s:%s" i j)) positions) (point)) (insert (format "%s%s" (emacspeak-table-this-element table i j) (if (= j count) "
" "	"))) (put-text-property column-start (point) (quote column) j) (setq column-start (point)) (incf j)) (setq j 0) (put-text-property row-start (point) (quote row) i) (setq row-start (point)) (incf i)) (emacspeak-table-mode) (goto-char (point-min)))) (switch-to-buffer buffer) (setq truncate-lines t) (message "Use Emacspeak Table UI to browse this table."))

(autoload (quote emacspeak-table-find-file) "emacspeak-table-ui" "\
Open a file containing table data and display it in table mode.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The etc/tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser." t nil)

(autoload (quote emacspeak-table-find-csv-file) "emacspeak-table-ui" "\
Process a csv (comma separated values) file. 
The processed  data and presented using emacspeak table navigation. " t nil)

(autoload (quote emacspeak-table-view-csv-buffer) "emacspeak-table-ui" "\
Process a csv (comma separated values) data. 
The processed  data and presented using emacspeak table
navigation. " t nil)

(autoload (quote emacspeak-table-display-table-in-region) "emacspeak-table-ui" "\
Recognize tabular data in current region and display it in table
browsing mode in a a separate buffer.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser." t nil)

(autoload (quote emacspeak-table-copy-to-clipboard) "emacspeak-table-ui" "\
Copy table in current buffer to the table clipboard.
Current buffer must be in emacspeak-table mode." t nil)

;;;***

;;;### (autoloads (ems-tabulate-parse-region emacspeak-tabulate-region)
;;;;;;  "emacspeak-tabulate" "emacspeak-tabulate.el" (16045 22386))
;;; Generated autoloads from emacspeak-tabulate.el

(autoload (quote emacspeak-tabulate-region) "emacspeak-tabulate" "\
Voicifies the white-space of a table if one found.  Optional interactive prefix
arg mark-fields specifies if the header row information is used to mark fields
in the white-space." t nil)

(autoload (quote ems-tabulate-parse-region) "emacspeak-tabulate" "\
Parse  region as tabular data and return a vector of vectors" nil nil)

;;;***

;;;### (autoloads (emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry"
;;;;;;  "emacspeak-tapestry.el" (16045 22386))
;;; Generated autoloads from emacspeak-tapestry.el

(autoload (quote emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry" "\
Describe the current layout of visible buffers in current frame.
Use interactive prefix arg to get coordinate positions of the
displayed buffers." t nil)

;;;***

;;;### (autoloads (emacspeak-url-template-fetch emacspeak-url-template-open
;;;;;;  emacspeak-url-template-load emacspeak-url-template-get) "emacspeak-url-template"
;;;;;;  "emacspeak-url-template.el" (16045 22388))
;;; Generated autoloads from emacspeak-url-template.el

(autoload (quote emacspeak-url-template-get) "emacspeak-url-template" "\
Lookup key and return corresponding template. " nil nil)

(autoload (quote emacspeak-url-template-load) "emacspeak-url-template" "\
Load URL template resources from specified location." t nil)

(autoload (quote emacspeak-url-template-open) "emacspeak-url-template" "\
Fetch resource identified by URL template." nil nil)

(autoload (quote emacspeak-url-template-fetch) "emacspeak-url-template" "\
Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource." t nil)

;;;***

;;;### (autoloads (emacspeak-w3-realaudio-play-url-at-point emacspeak-w3-browse-rss-at-point
;;;;;;  emacspeak-w3-preview-this-region emacspeak-w3-preview-this-buffer
;;;;;;  emacspeak-w3-google-similar-to-this-page emacspeak-w3-google-on-this-site
;;;;;;  emacspeak-w3-google-who-links-to-this-page emacspeak-w3-browse-xml-url-with-style
;;;;;;  emacspeak-w3-browse-url-with-style emacspeak-w3-xpath-filter-and-follow
;;;;;;  emacspeak-w3-class-filter-and-follow emacspeak-w3-extract-node-by-id
;;;;;;  emacspeak-w3-extract-by-class-list emacspeak-w3-extract-by-class
;;;;;;  emacspeak-w3-extract-tables-by-position-list emacspeak-w3-extract-table-by-position
;;;;;;  emacspeak-w3-extract-nested-table-list emacspeak-w3-extract-nested-table
;;;;;;  emacspeak-w3-extract-media-streams emacspeak-w3-xslt-filter
;;;;;;  emacspeak-w3-set-xsl-keep-result emacspeak-w3-count-tables
;;;;;;  emacspeak-w3-count-nested-tables emacspeak-w3-count-matches
;;;;;;  emacspeak-w3-xsl-toggle emacspeak-w3-xslt-select emacspeak-w3-xslt-apply)
;;;;;;  "emacspeak-w3" "emacspeak-w3.el" (16045 22392))
;;; Generated autoloads from emacspeak-w3.el

(autoload (quote emacspeak-w3-xslt-apply) "emacspeak-w3" "\
Apply specified transformation to current page." t nil)

(autoload (quote emacspeak-w3-xslt-select) "emacspeak-w3" "\
Select XSL transformation applied to WWW pages before they are displayed ." t nil)

(autoload (quote emacspeak-w3-xsl-toggle) "emacspeak-w3" "\
Toggle  application of XSL transformations.
This uses XSLT Processor xsltproc available as part of the
libxslt package." t nil)

(autoload (quote emacspeak-w3-count-matches) "emacspeak-w3" "\
Count matches for locator  in HTML." t nil)

(autoload (quote emacspeak-w3-count-nested-tables) "emacspeak-w3" "\
Count nested tables in HTML." t nil)

(autoload (quote emacspeak-w3-count-tables) "emacspeak-w3" "\
Count  tables in HTML." t nil)

(autoload (quote emacspeak-w3-set-xsl-keep-result) "emacspeak-w3" "\
Set value of `emacspeak-w3-xsl-keep-result'." t nil)

(autoload (quote emacspeak-w3-xslt-filter) "emacspeak-w3" "\
Extract elements matching specified XPath path locator
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract table from.  " t nil)

(autoload (quote emacspeak-w3-extract-media-streams) "emacspeak-w3" "\
Extract links to media streams.
operate on current web page when in a W3 buffer; otherwise
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically." t nil)

(autoload (quote emacspeak-w3-extract-nested-table) "emacspeak-w3" "\
Extract nested table specified by `table-index'. Default is to
operate on current web page when in a W3 buffer; otherwise
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically." t nil)

(autoload (quote emacspeak-w3-extract-nested-table-list) "emacspeak-w3" "\
Extract specified list of tables from a WWW page." t nil)

(autoload (quote emacspeak-w3-extract-table-by-position) "emacspeak-w3" "\
Extract table at specified position.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer." t nil)

(autoload (quote emacspeak-w3-extract-tables-by-position-list) "emacspeak-w3" "\
Extract specified list of nested tables from a WWW page.
Tables are specified by their position in the list 
nested of tables found in the page." t nil)

(autoload (quote emacspeak-w3-extract-by-class) "emacspeak-w3" "\
Extract elements having specified class attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer. Optional arg url specifies the page to extract content from.
Interactive use provides list of class values as completion." t nil)

(autoload (quote emacspeak-w3-extract-by-class-list) "emacspeak-w3" "\
Extract elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. " t nil)

(autoload (quote emacspeak-w3-extract-node-by-id) "emacspeak-w3" "\
Extract specified node from URI." t nil)

(autoload (quote emacspeak-w3-class-filter-and-follow) "emacspeak-w3" "\
Follow url and point, and filter the result by specified class.
Class can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well." t nil)

(autoload (quote emacspeak-w3-xpath-filter-and-follow) "emacspeak-w3" "\
Follow url and point, and filter the result by specified xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well." t nil)

(autoload (quote emacspeak-w3-browse-url-with-style) "emacspeak-w3" "\
Browse URL with specified XSL style." t nil)

(autoload (quote emacspeak-w3-browse-xml-url-with-style) "emacspeak-w3" "\
Browse XML URL with specified XSL style." t nil)

(autoload (quote emacspeak-w3-google-who-links-to-this-page) "emacspeak-w3" "\
Perform a google search to locate documents that link to the
current page." t nil)

(autoload (quote emacspeak-w3-google-on-this-site) "emacspeak-w3" "\
Perform a google search restricted to the current WWW site." t nil)

(autoload (quote emacspeak-w3-google-similar-to-this-page) "emacspeak-w3" "\
Ask Google to find documents similar to this one." t nil)

(autoload (quote emacspeak-w3-preview-this-buffer) "emacspeak-w3" "\
Preview this buffer." t nil)

(autoload (quote emacspeak-w3-preview-this-region) "emacspeak-w3" "\
Preview this region." t nil)

(autoload (quote emacspeak-w3-browse-rss-at-point) "emacspeak-w3" "\
Browses RSS url under point." t nil)

(autoload (quote emacspeak-w3-realaudio-play-url-at-point) "emacspeak-w3" "\
Play url under point as realaudio" t nil)

;;;***

;;;### (autoloads (emacspeak-websearch-usenet emacspeak-websearch-google
;;;;;;  emacspeak-websearch-emacspeak-archive emacspeak-websearch-do-post
;;;;;;  emacspeak-websearch-dispatch) "emacspeak-websearch" "emacspeak-websearch.el"
;;;;;;  (16045 22393))
;;; Generated autoloads from emacspeak-websearch.el

(defgroup emacspeak-websearch nil "Websearch tools for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-websearch-dispatch) "emacspeak-websearch" "\
Launches specific websearch queries.
Press `?' to list available search engines.
Once selected, the selected searcher prompts for additional information as appropriate.
When using W3,  this interface attempts to speak the most relevant information on the result page." t nil)

(autoload (quote emacspeak-websearch-do-post) "emacspeak-websearch" "\
Submit a post request. " nil nil)

(autoload (quote emacspeak-websearch-emacspeak-archive) "emacspeak-websearch" "\
Search Emacspeak mail archives.
For example to find messages about Redhat at the Emacspeak
archives, type +redhat" t nil)

(autoload (quote emacspeak-websearch-google) "emacspeak-websearch" "\
Perform an Google search.
Optional interactive prefix arg `lucky' is equivalent to hitting the 
I'm Feeling Lucky button on Google.
Meaning of the `lucky' flag can be inverted by setting option emacspeak-websearch-google-feeling-lucky-p." t nil)

(autoload (quote emacspeak-websearch-usenet) "emacspeak-websearch" "\
Prompt and browse a Usenet newsgroup.
Optional interactive prefix arg results in prompting for a search term." t nil)

;;;***

;;;### (autoloads (emacspeak-widget-default-summarize emacspeak-widget-summarize
;;;;;;  emacspeak-widget-summarize-parent) "emacspeak-widget" "emacspeak-widget.el"
;;;;;;  (16045 22393))
;;; Generated autoloads from emacspeak-widget.el

(autoload (quote emacspeak-widget-summarize-parent) "emacspeak-widget" "\
Summarize parent of widget at point." t nil)

(autoload (quote emacspeak-widget-summarize) "emacspeak-widget" "\
Summarize specified widget." nil nil)

(autoload (quote emacspeak-widget-default-summarize) "emacspeak-widget" "\
Fall back summarizer for all widgets" nil nil)

;;;***

;;;### (autoloads (emacspeak-wizards-generate-voice-sampler emacspeak-wizards-voice-sampler
;;;;;;  emacspeak-wizards-show-face emacspeak-wizards-find-longest-paragraph-in-region
;;;;;;  emacspeak-wizards-find-longest-line-in-region emacspeak-wizards-google-hits
;;;;;;  emacspeak-wizards-vc-viewer-refresh emacspeak-wizards-vc-viewer
;;;;;;  emacspeak-wizards-fix-read-only-text emacspeak-wizards-fix-typo
;;;;;;  emacspeak-wizards-spot-words emacspeak-kill-buffer-quietly
;;;;;;  emacspeak-switch-to-previous-buffer emacspeak-wizards-occur-header-lines
;;;;;;  emacspeak-wizards-how-many-matches emacspeak-wizards-count-slides-in-region
;;;;;;  emacspeak-wizards-squeeze-blanks emacspeak-wizards-show-environment-vvariable
;;;;;;  emacspeak-customize emacspeak-wizards-use-w3-or-w3m emacspeak-wizards-finder-find
;;;;;;  emacspeak-wizards-generate-finder emacspeak-wizards-portfolio-quotes
;;;;;;  emacspeak-wizards-ppt-display emacspeak-wizards-xl-display
;;;;;;  emacspeak-wizards-rpm-query-in-dired emacspeak-wizards-shell-toggle
;;;;;;  emacspeak-annotate-add-annotation emacspeak-wizards-get-table-content-from-file
;;;;;;  emacspeak-wizards-get-table-content-from-url emacspeak-lynx
;;;;;;  emacspeak-links emacspeak-skip-blank-lines-backward emacspeak-skip-blank-lines-forward
;;;;;;  emacspeak-show-property-at-point emacspeak-show-personality-at-point
;;;;;;  emacspeak-customize-personal-settings emacspeak-emergency-tts-restart
;;;;;;  emacspeak-speak-show-memory-used emacspeak-wizards-show-list-variable
;;;;;;  emacspeak-clipboard-paste emacspeak-clipboard-copy emacspeak-select-this-buffer-next-display
;;;;;;  emacspeak-select-this-buffer-previous-display emacspeak-select-this-buffer-other-window-display
;;;;;;  emacspeak-speak-this-buffer-next-display emacspeak-speak-this-buffer-previous-display
;;;;;;  emacspeak-speak-this-buffer-other-window-display emacspeak-previous-frame-or-buffer
;;;;;;  emacspeak-next-frame-or-buffer emacspeak-generate-texinfo-command-documentation
;;;;;;  emacspeak-generate-documentation emacspeak-learn-mode emacspeak-cvs-gnu-get-project-snapshot
;;;;;;  emacspeak-cvs-sf-get-project-snapshot emacspeak-cvs-get-anonymous
;;;;;;  emacspeak-sudo emacspeak-root emacspeak-speak-telephone-directory
;;;;;;  emacspeak-speak-show-active-network-interfaces emacspeak-speak-hostname
;;;;;;  emacspeak-speak-popup-messages emacspeak-speak-browse-linux-howto
;;;;;;  emacspeak-speak-load-directory-settings emacspeak-speak-run-shell-command
;;;;;;  emacspeak-symlink-current-file emacspeak-link-current-file
;;;;;;  emacspeak-copy-current-file emacspeak-view-emacspeak-faq
;;;;;;  emacspeak-view-emacspeak-tips emacspeak-view-emacspeak-doc)
;;;;;;  "emacspeak-wizards" "emacspeak-wizards.el" (16045 22394))
;;; Generated autoloads from emacspeak-wizards.el

(autoload (quote emacspeak-view-emacspeak-doc) "emacspeak-wizards" "\
Display a summary of all Emacspeak commands." t nil)

(autoload (quote emacspeak-view-emacspeak-tips) "emacspeak-wizards" "\
Browse  Emacspeak productivity tips." t nil)

(autoload (quote emacspeak-view-emacspeak-faq) "emacspeak-wizards" "\
Browse the Emacspeak FAQ." t nil)

(autoload (quote emacspeak-copy-current-file) "emacspeak-wizards" "\
Copy file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when copying.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Asks for confirmation if the copy will result in an
  existing file being overwritten." t nil)

(autoload (quote emacspeak-link-current-file) "emacspeak-wizards" "\
Link (hard link) file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when linking.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Signals an error if target already exists." t nil)

(autoload (quote emacspeak-symlink-current-file) "emacspeak-wizards" "\
Link (symbolic link) file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when linking.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Signals an error if target already exists." t nil)

(autoload (quote emacspeak-speak-run-shell-command) "emacspeak-wizards" "\
Invoke shell COMMAND and display its output as a table.  The results
are placed in a buffer in Emacspeak's table browsing mode.  Optional
interactive prefix arg as-root runs the command as root (not yet
implemented).  Use this for running shell commands that produce
tabulated output.  This command should be used for shell commands that
produce tabulated output that works with Emacspeak's table recognizer.
Verify this first by running the command in a shell and executing
command `emacspeak-table-display-table-in-region' normally bound to
\\[emacspeak-table-display-table-in-region]." t nil)

(autoload (quote emacspeak-speak-load-directory-settings) "emacspeak-wizards" "\
Load a directory specific Emacspeak settings file.
This is typically used to load up settings that are specific to
an electronic book consisting of many files in the same
directory." t nil)

(autoload (quote emacspeak-speak-browse-linux-howto) "emacspeak-wizards" "\
Browse a Linux Howto file.
We cleanup underlining, and set up outline mode correctly." t nil)

(autoload (quote emacspeak-speak-popup-messages) "emacspeak-wizards" "\
Pop up messages buffer.
If it is already selected then hide it and try to restore
previous window configuration." t nil)

(autoload (quote emacspeak-speak-hostname) "emacspeak-wizards" "\
Speak host name." t nil)

(autoload (quote emacspeak-speak-show-active-network-interfaces) "emacspeak-wizards" "\
Shows all active network interfaces in the echo area.
With interactive prefix argument ADDRESS it prompts for a
specific interface and shows its address. The address is
also copied to the kill ring for convenient yanking." t nil)

(autoload (quote emacspeak-speak-telephone-directory) "emacspeak-wizards" "\
Lookup and display a phone number.
With prefix arg, opens the phone book for editting." t nil)

(autoload (quote emacspeak-root) "emacspeak-wizards" "\
Start a root shell or switch to one that already exists.
Optional interactive prefix arg `cd' executes cd
default-directory after switching." t nil)

(autoload (quote emacspeak-sudo) "emacspeak-wizards" "\
SUDo command --run command as super user." t nil)

(autoload (quote emacspeak-cvs-get-anonymous) "emacspeak-wizards" "\
Get latest cvs snapshot of emacspeak." t nil)

(autoload (quote emacspeak-cvs-sf-get-project-snapshot) "emacspeak-wizards" "\
Grab CVS snapshot  of specified project from Sourceforge." t nil)

(autoload (quote emacspeak-cvs-gnu-get-project-snapshot) "emacspeak-wizards" "\
Grab CVS snapshot  of specified project from GNU." t nil)

(autoload (quote emacspeak-learn-mode) "emacspeak-wizards" "\
Helps you learn the keys.  You can press keys and hear what they do.
To leave, press \\[keyboard-quit]." t nil)

(autoload (quote emacspeak-generate-documentation) "emacspeak-wizards" "\
Generate docs for all emacspeak commands.
Prompts for FILENAME in which to save the documentation.
Warning! Contents of file filename will be overwritten." t nil)

(autoload (quote emacspeak-generate-texinfo-command-documentation) "emacspeak-wizards" "\
Generate texinfo documentation  for all emacspeak
commands into file commands.texi.
Warning! Contents of file commands.texi will be overwritten." t nil)

(defsubst emacspeak-frame-read-frame-label nil "\
Read a frame label with completion." (interactive) (let* ((frame-names-alist (make-frame-names-alist)) (default (car (car frame-names-alist))) (input (completing-read (format "Select Frame (default %s): " default) frame-names-alist nil t nil (quote frame-name-history)))) (if (= (length input) 0) default)))

(autoload (quote emacspeak-next-frame-or-buffer) "emacspeak-wizards" "\
Move to next buffer.
With optional interactive prefix arg `frame', move to next frame instead." t nil)

(autoload (quote emacspeak-previous-frame-or-buffer) "emacspeak-wizards" "\
Move to previous buffer.
With optional interactive prefix arg `frame', move to previous frame instead." t nil)

(autoload (quote emacspeak-speak-this-buffer-other-window-display) "emacspeak-wizards" "\
Speak this buffer as displayed in a different frame.  Emacs
allows you to display the same buffer in multiple windows or
frames.  These different windows can display different
portions of the buffer.  This is equivalent to leaving a
book open at places at once.  This command allows you to
listen to the places where you have left the book open.  The
number used to invoke this command specifies which of the
displays you wish to speak.  Typically you will have two or
at most three such displays open.  The current display is 0,
the next is 1, and so on.  Optional argument ARG specifies
the display to speak." t nil)

(autoload (quote emacspeak-speak-this-buffer-previous-display) "emacspeak-wizards" "\
Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `previous'." t nil)

(autoload (quote emacspeak-speak-this-buffer-next-display) "emacspeak-wizards" "\
Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `next'." t nil)

(autoload (quote emacspeak-select-this-buffer-other-window-display) "emacspeak-wizards" "\
Switch  to this buffer as displayed in a different frame.  Emacs
allows you to display the same buffer in multiple windows or
frames.  These different windows can display different
portions of the buffer.  This is equivalent to leaving a
book open at places at once.  This command allows you to
move to the places where you have left the book open.  The
number used to invoke this command specifies which of the
displays you wish to select.  Typically you will have two or
at most three such displays open.  The current display is 0,
the next is 1, and so on.  Optional argument ARG specifies
the display to select." t nil)

(autoload (quote emacspeak-select-this-buffer-previous-display) "emacspeak-wizards" "\
Select this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-select-this-buffer-other-window-display' for the
meaning of `previous'." t nil)

(autoload (quote emacspeak-select-this-buffer-next-display) "emacspeak-wizards" "\
Select this buffer as displayed in a `next' frame.
See documentation for command
`emacspeak-select-this-buffer-other-window-display' for the
meaning of `next'." t nil)

(autoload (quote emacspeak-clipboard-copy) "emacspeak-wizards" "\
Copy contents of the region to the emacspeak clipboard.
Previous contents of the clipboard will be overwritten.  The Emacspeak
clipboard is a convenient way of sharing information between
independent Emacspeak sessions running on the same or different
machines.  Do not use this for sharing information within an Emacs
session --Emacs' register commands are far more efficient and
light-weight.  Optional interactive prefix arg results in Emacspeak
prompting for the clipboard file to use.
Argument START and END specifies  region.
Optional argument PROMPT  specifies whether we prompt for the name of a clipboard file." t nil)

(autoload (quote emacspeak-clipboard-paste) "emacspeak-wizards" "\
Yank contents of the Emacspeak clipboard at point.
The Emacspeak clipboard is a convenient way of sharing information between
independent Emacspeak sessions running on the same or different
machines.  Do not use this for sharing information within an Emacs
session --Emacs' register commands are far more efficient and
light-weight.  Optional interactive prefix arg pastes from
the emacspeak table clipboard instead." t nil)

(autoload (quote emacspeak-wizards-show-list-variable) "emacspeak-wizards" "\
Convenience command to view Emacs variables that are long lists.
Prompts for a variable name and displays its value in a separate buffer.
Lists are displayed one element per line.
Argument VAR specifies variable whose value is to be displayed." t nil)

(autoload (quote emacspeak-speak-show-memory-used) "emacspeak-wizards" "\
Convenience command to view state of memory used in this session so far." t nil)

(autoload (quote emacspeak-emergency-tts-restart) "emacspeak-wizards" "\
For use in an emergency.
Will start TTS engine specified by 
emacspeak-emergency-tts-server." t nil)

(autoload (quote emacspeak-customize-personal-settings) "emacspeak-wizards" "\
Create a customization buffer for browsing and updating
personal customizations." t nil)

(autoload (quote emacspeak-show-personality-at-point) "emacspeak-wizards" "\
Show value of property personality (and possibly face)
at point." t nil)

(autoload (quote emacspeak-show-property-at-point) "emacspeak-wizards" "\
Show value of PROPERTY at point.
If optional arg property is not supplied, read it interactively.
Provides completion based on properties that are of interest.
If no property is set, show a message and exit." t nil)

(autoload (quote emacspeak-skip-blank-lines-forward) "emacspeak-wizards" "\
Move forward across blank lines.
The line under point is then spoken.
Signals end of buffer." t nil)

(autoload (quote emacspeak-skip-blank-lines-backward) "emacspeak-wizards" "\
Move backward  across blank lines.
The line under point is   then spoken.
Signals beginning  of buffer." t nil)

(autoload (quote emacspeak-links) "emacspeak-wizards" "\
Launch links on  specified URL in a new terminal." t nil)

(autoload (quote emacspeak-lynx) "emacspeak-wizards" "\
Launch lynx on  specified URL in a new terminal." t nil)

(autoload (quote emacspeak-wizards-get-table-content-from-url) "emacspeak-wizards" "\
Extract table specified by depth and count from HTML
content at URL.
Extracted content is placed as a csv file in task.csv." t nil)

(autoload (quote emacspeak-wizards-get-table-content-from-file) "emacspeak-wizards" "\
Extract table specified by depth and count from HTML
content at file.
Extracted content is placed as a csv file in task.csv." t nil)

(autoload (quote emacspeak-annotate-add-annotation) "emacspeak-wizards" "\
Add annotation to the annotation working buffer.
Prompt for annotation buffer if not already set.
Interactive prefix arg `reset' prompts for the annotation
buffer even if one is already set.
Annotation is entered in a temporary buffer and the
annotation is inserted into the working buffer when complete." t nil)

(autoload (quote emacspeak-wizards-shell-toggle) "emacspeak-wizards" "\
Switch to the shell buffer and cd to 
 the directory of the current buffer." t nil)

(autoload (quote emacspeak-wizards-rpm-query-in-dired) "emacspeak-wizards" "\
Run rpm -qi on current dired entry." t nil)

(autoload (quote emacspeak-wizards-xl-display) "emacspeak-wizards" "\
Called to set up preview of an XL file.
Assumes we are in a buffer visiting a .xls file.
Previews those contents as HTML and nukes the buffer
visiting the xls file." t nil)

(autoload (quote emacspeak-wizards-ppt-display) "emacspeak-wizards" "\
Called to set up preview of an PPT file.
Assumes we are in a buffer visiting a .ppt file.
Previews those contents as HTML and nukes the buffer
visiting the ppt file." t nil)

(autoload (quote emacspeak-wizards-portfolio-quotes) "emacspeak-wizards" "\
Bring up detailed stock quotes for portfolio specified by 
emacspeak-websearch-personal-portfolio." t nil)

(autoload (quote emacspeak-wizards-generate-finder) "emacspeak-wizards" "\
Generate a widget-enabled finder wizard." t nil)

(autoload (quote emacspeak-wizards-finder-find) "emacspeak-wizards" "\
Run find-dired on specified switches after prompting for the
directory to where find is to be launched." t nil)

(autoload (quote emacspeak-wizards-use-w3-or-w3m) "emacspeak-wizards" "\
Alternates between using W3 and W3M for browse-url." t nil)

(autoload (quote emacspeak-customize) "emacspeak-wizards" "\
Customize Emacspeak." t nil)

(autoload (quote emacspeak-wizards-show-environment-vvariable) "emacspeak-wizards" "\
Display value of specified environment variable." t nil)

(autoload (quote emacspeak-wizards-squeeze-blanks) "emacspeak-wizards" "\
Squeeze multiple blank lines in current buffer." t nil)

(autoload (quote emacspeak-wizards-count-slides-in-region) "emacspeak-wizards" "\
Count slides starting from point." t nil)

(autoload (quote emacspeak-wizards-how-many-matches) "emacspeak-wizards" "\
If you define a file local variable 
called `emacspeak-occur-pattern' that holds a regular expression 
that matches  lines of interest, you can use this command to conveniently
run `how-many'to count  matching header lines.
With interactive prefix arg, prompts for and remembers the file local pattern." t nil)

(autoload (quote emacspeak-wizards-occur-header-lines) "emacspeak-wizards" "\
If you define a file local variable 
called `emacspeak-occur-pattern' that holds a regular expression 
that matches header lines, you can use this command to conveniently
run `occur' 
to find matching header lines. With prefix arg, prompts for and sets
value of the file local pattern." t nil)

(autoload (quote emacspeak-switch-to-previous-buffer) "emacspeak-wizards" "\
Switch to most recently used interesting buffer." t nil)

(autoload (quote emacspeak-kill-buffer-quietly) "emacspeak-wizards" "\
Kill current buffer without asking for confirmation." t nil)

(autoload (quote emacspeak-wizards-spot-words) "emacspeak-wizards" "\
Searches recursively in all files with extension `ext'
for `word' and displays hits in a compilation buffer." t nil)

(autoload (quote emacspeak-wizards-fix-typo) "emacspeak-wizards" "\
Search and replace  recursively in all files with extension `ext'
for `word' and replace it with correction.
Use with caution." t nil)

(autoload (quote emacspeak-wizards-fix-read-only-text) "emacspeak-wizards" "\
Nuke read-only property on text range." t nil)

(autoload (quote emacspeak-wizards-vc-viewer) "emacspeak-wizards" "\
View contents of specified virtual console." t nil)

(autoload (quote emacspeak-wizards-vc-viewer-refresh) "emacspeak-wizards" "\
Refresh view of VC we're viewing." t nil)

(autoload (quote emacspeak-wizards-google-hits) "emacspeak-wizards" "\
Filter Google results after performing search to show just the
hits." t nil)

(autoload (quote emacspeak-wizards-find-longest-line-in-region) "emacspeak-wizards" "\
Find longest line in region.
Moves to the longest line when called interactively." t nil)

(autoload (quote emacspeak-wizards-find-longest-paragraph-in-region) "emacspeak-wizards" "\
Find longest paragraph in region.
Moves to the longest paragraph when called interactively." t nil)

(autoload (quote emacspeak-wizards-show-face) "emacspeak-wizards" "\
Show salient properties of specified face." t nil)

(autoload (quote emacspeak-wizards-voice-sampler) "emacspeak-wizards" "\
Read a personality  and apply it to the current line." t nil)

(autoload (quote emacspeak-wizards-generate-voice-sampler) "emacspeak-wizards" "\
Generate a buffer that shows a sample line in all the ACSS settings
for the current voice family." t nil)

;;;***

;;;### (autoloads (emacspeak-xml-shell) "emacspeak-xml-shell" "emacspeak-xml-shell.el"
;;;;;;  (16045 22397))
;;; Generated autoloads from emacspeak-xml-shell.el

(defgroup emacspeak-xml-shell nil "XML browser for the Emacspeak desktop.")

(autoload (quote emacspeak-xml-shell) "emacspeak-xml-shell" "\
Start Xml-Shell on contents of system-id." t nil)

;;;***

;;;### (autoloads (emacspeak-xslt-xml-url emacspeak-xslt-url emacspeak-xslt-region)
;;;;;;  "emacspeak-xslt" "emacspeak-xslt.el" (16045 22398))
;;; Generated autoloads from emacspeak-xslt.el

(defvar emacspeak-xslt-directory (expand-file-name "xsl/" emacspeak-directory) "\
Directory holding XSL transformations.")

(autoload (quote emacspeak-xslt-region) "emacspeak-xslt" "\
Apply XSLT transformation to region and replace it with
the result.  This uses XSLT processor xsltproc available as
part of the libxslt package." nil nil)

(autoload (quote emacspeak-xslt-url) "emacspeak-xslt" "\
Apply XSLT transformation to url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package." nil nil)

(autoload (quote emacspeak-xslt-xml-url) "emacspeak-xslt" "\
Apply XSLT transformation to XML url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package." nil nil)

;;;***

;;;### (autoloads (turn-on-voice-lock voice-lock-mode) "voice-setup"
;;;;;;  "voice-setup.el" (16045 22400))
;;; Generated autoloads from voice-setup.el

(autoload (quote voice-lock-mode) "voice-setup" "\
Toggle Voice Lock mode.
With arg, turn Voice Lock mode on if and only if arg is positive.

This light-weight voice lock engine leverages work already done by
font-lock.  Voicification is effective only if font lock is on." t nil)

(autoload (quote turn-on-voice-lock) "voice-setup" "\
Turn on Voice Lock mode ." nil nil)

;;;***

;;;### (autoloads (xml-reformat-tags insert-xml read-xml) "xml-parse"
;;;;;;  "xml-parse.el" (16044 27431))
;;; Generated autoloads from xml-parse.el

(autoload (quote read-xml) "xml-parse" "\
Parse XML data at point into a Lisp structure.
See `insert-xml' for a description of the format of this structure.
Point is left at the end of the XML structure read." nil nil)

(autoload (quote insert-xml) "xml-parse" "\
Insert DATA, a recursive Lisp structure, at point as XML.
DATA has the form:

  ENTRY       ::=  (TAG CHILD*)
  CHILD       ::=  STRING | ENTRY
  TAG         ::=  TAG_NAME | (TAG_NAME ATTR+)
  ATTR        ::=  (ATTR_NAME . ATTR_VALUE)
  TAG_NAME    ::=  STRING
  ATTR_NAME   ::=  STRING
  ATTR_VALUE  ::=  STRING

If ADD-NEWLINES is non-nil, newlines and indentation will be added to
make the data user-friendly.

If PUBLIC and SYSTEM are non-nil, a !DOCTYPE tag will be added at the
top of the document to identify it as an XML document.

DEPTH is normally for internal use only, and controls the depth of the
indentation." nil nil)

(autoload (quote xml-reformat-tags) "xml-parse" "\
If point is on the open bracket of an XML tag, reformat that tree.
Note that this only works if the opening tag starts at column 0." t nil)

;;;***

;;;### (autoloads (amphetadesk) "amphetadesk" "amphetadesk.el" (16045
;;;;;;  22314))
;;; Generated autoloads from amphetadesk.el

(autoload (quote amphetadesk) "amphetadesk" "\
Open amphetadesk." t nil)

;;;***
