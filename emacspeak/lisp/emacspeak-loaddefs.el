;;;Auto generated

;;;### (autoloads (cd-tool) "cd-tool" "cd-tool.el" (15896 27064))
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

;;;### (autoloads (tts-eflite) "eflite" "eflite.el" (15933 51396))
;;; Generated autoloads from eflite.el

(autoload (quote tts-eflite) "eflite" "\
Use eflite TTS server." t nil)

;;;***

;;;### (autoloads (emacspeak-aumix-volume-decrease emacspeak-aumix-volume-increase
;;;;;;  emacspeak-aumix-wave-decrease emacspeak-aumix-wave-increase
;;;;;;  emacspeak-aumix-reset-options emacspeak-aumix-settings-file)
;;;;;;  "emacspeak-aumix" "emacspeak-aumix.el" (15896 27067))
;;; Generated autoloads from emacspeak-aumix.el

(defgroup emacspeak-aumix nil "Customization group for setting the Emacspeak auditory\ndisplay." :group (quote emacspeak))

(defvar emacspeak-aumix-settings-file (when (file-exists-p (expand-file-name ".aumixrc" emacspeak-resource-directory)) (expand-file-name ".aumixrc" emacspeak-resource-directory)) "\
*Name of file containing personal aumix settings.")

(defvar emacspeak-aumix-reset-options (format "-f %s -L 2>&1 >/dev/null" emacspeak-aumix-settings-file) "\
*Option to pass to aumix for resetting to default values.")

(autoload (quote emacspeak-aumix-wave-increase) "emacspeak-aumix" "\
Increase volume of wave output. " t nil)

(autoload (quote emacspeak-aumix-wave-decrease) "emacspeak-aumix" "\
Decrease volume of wave output. " t nil)

(autoload (quote emacspeak-aumix-volume-increase) "emacspeak-aumix" "\
Increase overall volume. " t nil)

(autoload (quote emacspeak-aumix-volume-decrease) "emacspeak-aumix" "\
Decrease overall volume. " t nil)

;;;***

;;;### (autoloads (emacspeak-daisy-open-book) "emacspeak-daisy" "emacspeak-daisy.el"
;;;;;;  (15898 454))
;;; Generated autoloads from emacspeak-daisy.el

(defgroup emacspeak-daisy nil "Daisy Digital Talking Books  for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-daisy-open-book) "emacspeak-daisy" "\
Open Digital Talking Book specified by navigation file filename." t nil)

;;;***

;;;### (autoloads (emacspeak-eterm-remote-term) "emacspeak-eterm"
;;;;;;  "emacspeak-eterm.el" (15947 59010))
;;; Generated autoloads from emacspeak-eterm.el

(defgroup emacspeak-eterm nil "Terminal emulator for the Emacspeak Desktop." :group (quote emacspeak) :prefix "emacspeak-eterm-")

(autoload (quote emacspeak-eterm-remote-term) "emacspeak-eterm" "\
Start a terminal-emulator in a new buffer." t nil)

;;;***

;;;### (autoloads (emacspeak-filtertext) "emacspeak-filtertext" "emacspeak-filtertext.el"
;;;;;;  (15896 27071))
;;; Generated autoloads from emacspeak-filtertext.el

(autoload (quote emacspeak-filtertext) "emacspeak-filtertext" "\
Copy over text in region to special filtertext buffer in
preparation for interactively filtering text. " t nil)

;;;***

;;;### (autoloads (emacspeak-forms-find-file) "emacspeak-forms" "emacspeak-forms.el"
;;;;;;  (15896 27073))
;;; Generated autoloads from emacspeak-forms.el

(autoload (quote emacspeak-forms-find-file) "emacspeak-forms" "\
Visit a forms file" t nil)

;;;***

;;;### (autoloads (emacspeak-freeamp emacspeak-freeamp-freeamp-call-command
;;;;;;  emacspeak-freeamp-freeamp-command) "emacspeak-freeamp" "emacspeak-freeamp.el"
;;;;;;  (15923 6573))
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
;;;;;;  (15947 59011))
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
;;;;;;  (15949 43353))
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
;;;;;;  (15947 59011))
;;; Generated autoloads from emacspeak-imcom.el

(defgroup emacspeak-imcom nil "Jabber access from the Emacspeak audio desktop.")

(autoload (quote emacspeak-imcom) "emacspeak-imcom" "\
Start IMCom." t nil)

;;;***

;;;### (autoloads (emacspeak-m-player) "emacspeak-m-player" "emacspeak-m-player.el"
;;;;;;  (15896 27075))
;;; Generated autoloads from emacspeak-m-player.el

(defgroup emacspeak-m-player nil "Emacspeak media player settings.")

(autoload (quote emacspeak-m-player) "emacspeak-m-player" "\
Play specified resource using m-player.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-m-player-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-ocr) "emacspeak-ocr" "emacspeak-ocr.el"
;;;;;;  (15947 59013))
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

;;;### (autoloads (voice-lock-mode) "emacspeak-personality" "emacspeak-personality.el"
;;;;;;  (15949 43354))
;;; Generated autoloads from emacspeak-personality.el

(autoload (quote voice-lock-mode) "emacspeak-personality" "\
Toggle Voice Lock mode.
With arg, turn Voice Lock mode on if and only if arg is positive.

This light-weight voice lock engine leverages work already done by
font-lock.  Voicification is effective only if font lock is on." t nil)

;;;***

;;;### (autoloads (emacspeak-realaudio-browse emacspeak-realaudio
;;;;;;  emacspeak-realaudio-process-sentinel emacspeak-realaudio-play)
;;;;;;  "emacspeak-realaudio" "emacspeak-realaudio.el" (15898 461))
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

;;;### (autoloads (emacspeak-remote-connect-to-server emacspeak-remote-ssh-to-server
;;;;;;  emacspeak-remote-quick-connect-to-server) "emacspeak-remote"
;;;;;;  "emacspeak-remote.el" (15896 27077))
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
;;;;;;  "emacspeak-rss.el" (15902 12478))
;;; Generated autoloads from emacspeak-rss.el

(defgroup emacspeak-rss nil "RSS Feeds for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-rss-display) "emacspeak-rss" "\
Retrieve and display RSS news feed." t nil)

(autoload (quote emacspeak-rss-browse) "emacspeak-rss" "\
Browse specified RSS feed." t nil)

;;;***

;;;### (autoloads nil "emacspeak-setup" "emacspeak-setup.el" (15896
;;;;;;  64243))
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
;;;;;;  emacspeak-sounds-select-theme emacspeak-play-program emacspeak-sounds-default-theme)
;;;;;;  "emacspeak-sounds" "emacspeak-sounds.el" (15896 64215))
;;; Generated autoloads from emacspeak-sounds.el

(defvar emacspeak-sounds-default-theme (expand-file-name "default-8k/" emacspeak-sounds-directory) "\
Default theme for auditory icons. ")

(defvar emacspeak-play-program (cond ((getenv "EMACSPEAK_PLAY_PROGRAM")) ((file-exists-p "/usr/bin/play") "/usr/bin/play") ((file-exists-p "/usr/bin/audioplay") "/usr/bin/audioplay") ((file-exists-p "/usr/demo/SOUND/play") "/usr/demo/SOUND/play") (t (expand-file-name emacspeak-etc-directory "play"))) "\
Name of executable that plays sound files. ")

(autoload (quote emacspeak-sounds-select-theme) "emacspeak-sounds" "\
Select theme for auditory icons." t nil)

(autoload (quote emacspeak-toggle-auditory-icons) "emacspeak-sounds" "\
Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value." t nil)

(autoload (quote emacspeak-set-auditory-icon-player) "emacspeak-sounds" "\
Select  player used for producing auditory icons.
Recommended choices:

emacspeak-serve-auditory-icon for  the wave device.
emacspeak-midi-icon for midi device. " t nil)

;;;***

;;;### (autoloads (emacspeak-toggle-comint-output-monitor) "emacspeak-speak"
;;;;;;  "emacspeak-speak.el" (15947 59014))
;;; Generated autoloads from emacspeak-speak.el

(autoload (quote emacspeak-toggle-comint-output-monitor) "emacspeak-speak" "\
Toggle state of Emacspeak comint monitor.
When turned on, comint output is automatically spoken.  Turn this on if
you want your shell to speak its results.  Interactive
PREFIX arg means toggle the global default value, and then
set the current local value to the result." t nil)

;;;***

;;;### (autoloads (emacspeak-table-copy-to-clipboard emacspeak-table-display-table-in-region
;;;;;;  emacspeak-table-view-csv-buffer emacspeak-table-find-csv-file
;;;;;;  emacspeak-table-find-file) "emacspeak-table-ui" "emacspeak-table-ui.el"
;;;;;;  (15896 27079))
;;; Generated autoloads from emacspeak-table-ui.el

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

;;;### (autoloads (emacspeak-tabulate-region) "emacspeak-tabulate"
;;;;;;  "emacspeak-tabulate.el" (15896 27079))
;;; Generated autoloads from emacspeak-tabulate.el

(autoload (quote emacspeak-tabulate-region) "emacspeak-tabulate" "\
Voicifies the white-space of a table if one found.  Optional interactive prefix
arg mark-fields specifies if the header row information is used to mark fields
in the white-space." t nil)

;;;***

;;;### (autoloads (emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry"
;;;;;;  "emacspeak-tapestry.el" (15900 37057))
;;; Generated autoloads from emacspeak-tapestry.el

(autoload (quote emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry" "\
Describe the current layout of visible buffers in current frame.
Use interactive prefix arg to get coordinate positions of the
displayed buffers." t nil)

;;;***

;;;### (autoloads (emacspeak-url-template-fetch emacspeak-url-template-load)
;;;;;;  "emacspeak-url-template" "emacspeak-url-template.el" (15902
;;;;;;  13149))
;;; Generated autoloads from emacspeak-url-template.el

(autoload (quote emacspeak-url-template-load) "emacspeak-url-template" "\
Load URL template resources from specified location." t nil)

(autoload (quote emacspeak-url-template-fetch) "emacspeak-url-template" "\
Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource." t nil)

;;;***

;;;### (autoloads (emacspeak-w3-realaudio-play-url-at-point) "emacspeak-w3"
;;;;;;  "emacspeak-w3.el" (15946 36816))
;;; Generated autoloads from emacspeak-w3.el

(autoload (quote emacspeak-w3-realaudio-play-url-at-point) "emacspeak-w3" "\
Play url under point as realaudio" t nil)

;;;***

;;;### (autoloads (emacspeak-websearch-usenet emacspeak-websearch-google
;;;;;;  emacspeak-websearch-emacspeak-archive emacspeak-websearch-dispatch)
;;;;;;  "emacspeak-websearch" "emacspeak-websearch.el" (15947 59015))
;;; Generated autoloads from emacspeak-websearch.el

(defgroup emacspeak-websearch nil "Websearch tools for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-websearch-dispatch) "emacspeak-websearch" "\
Launches specific websearch queries.
Press `?' to list available search engines.
Once selected, the selected searcher prompts for additional information as appropriate.
When using W3,  this interface attempts to speak the most relevant information on the result page." t nil)

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

;;;### (autoloads (emacspeak-xml-shell) "emacspeak-xml-shell" "emacspeak-xml-shell.el"
;;;;;;  (15947 59016))
;;; Generated autoloads from emacspeak-xml-shell.el

(defgroup emacspeak-xml-shell nil "XML browser for the Emacspeak desktop.")

(autoload (quote emacspeak-xml-shell) "emacspeak-xml-shell" "\
Start Xml-Shell on contents of system-id." t nil)

;;;***

;;;### (autoloads (turn-on-fast-voice-lock fast-voice-lock-mode)
;;;;;;  "fast-voice-lock" "fast-voice-lock.el" (15896 27083))
;;; Generated autoloads from fast-voice-lock.el

(autoload (quote fast-voice-lock-mode) "fast-voice-lock" "\
Toggle Fast Lock mode.
With arg, turn Fast Lock mode on if and only if arg is positive and the buffer
is associated with a file.  Enable it automatically in your `~/.emacs' by:

 (setq voice-lock-support-mode 'fast-voice-lock-mode)

If Fast Lock mode is enabled, and the current buffer does not contain any text
properties, any associated Voice Lock cache is used if its timestamp matches the
buffer's file, and its `voice-lock-keywords' match those that you are using.

Voice Lock caches may be saved:
 - When you save the file's buffer.
 - When you kill an unmodified file's buffer.
 - When you exit Emacs, for all unmodified or saved buffers.
Depending on the value of `fast-voice-lock-save-events'.
See also the commands `fast-voice-lock-read-cache' and `fast-voice-lock-save-cache'.

Use \\[voice-lock-voiceify-buffer] to voiceify the buffer if the cache is bad.

Various methods of control are provided for the Voice Lock cache.  In general,
see variable `fast-voice-lock-cache-directories' and function `fast-voice-lock-cache-name'.
For saving, see variables `fast-voice-lock-minimum-size', `fast-voice-lock-save-events',
`fast-voice-lock-save-others' and `fast-voice-lock-save-personalities'.

Use \\[fast-voice-lock-submit-bug-report] to send bug reports or feedback." t nil)

(autoload (quote turn-on-fast-voice-lock) "fast-voice-lock" "\
Unconditionally turn on Fast Lock mode." nil nil)

(unless (assq (quote fast-voice-lock-mode) minor-mode-alist) (setq minor-mode-alist (append minor-mode-alist (quote ((fast-voice-lock-mode nil))))))

;;;***

;;;### (autoloads (regexp-opt-depth regexp-opt) "regexp-opt" "regexp-opt.el"
;;;;;;  (15896 27084))
;;; Generated autoloads from regexp-opt.el

(autoload (quote regexp-opt) "regexp-opt" "\
Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct.
The returned regexp is typically more efficient than the equivalent regexp:

 (let ((open-paren (if PAREN \"\\\\(\" \"\")) (close-paren (if PAREN \"\\\\)\" \"\")))
   (concat open-paren (mapconcat 'regexp-quote STRINGS \"\\\\|\") close-paren))

but typically contains more regexp grouping constructs.
Use `regexp-opt-depth' to count them." nil nil)

(autoload (quote regexp-opt-depth) "regexp-opt" "\
Return the depth of REGEXP.
This means the number of regexp grouping constructs (parenthesised expressions)
in REGEXP." nil nil)

;;;***

;;;### (autoloads (xml-reformat-tags insert-xml read-xml) "xml-parse"
;;;;;;  "xml-parse.el" (15895 42047))
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

;;;### (autoloads (amphetadesk) "amphetadesk" "amphetadesk.el" (15905
;;;;;;  38426))
;;; Generated autoloads from amphetadesk.el

(autoload (quote amphetadesk) "amphetadesk" "\
Open amphetadesk." t nil)

;;;***
