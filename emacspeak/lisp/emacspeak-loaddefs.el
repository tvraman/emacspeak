;;;Auto generated

;;;### (autoloads (emacspeak-aumix-volume-decrease emacspeak-aumix-volume-increase
;;;;;;  emacspeak-aumix-wave-decrease emacspeak-aumix-wave-increase
;;;;;;  emacspeak-aumix-reset-options emacspeak-aumix-settings-file)
;;;;;;  "emacspeak-aumix" "emacspeak-aumix.el" (15893 49909))
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
;;;;;;  (15893 49008))
;;; Generated autoloads from emacspeak-daisy.el

(defgroup emacspeak-daisy nil "Daisy Digital Talking Books  for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-daisy-open-book) "emacspeak-daisy" "\
Open Digital Talking Book specified by navigation file filename." t nil)

;;;***

;;;### (autoloads (emacspeak-eterm-remote-term) "emacspeak-eterm"
;;;;;;  "emacspeak-eterm.el" (15893 49008))
;;; Generated autoloads from emacspeak-eterm.el

(defgroup emacspeak-eterm nil "Terminal emulator for the Emacspeak Desktop." :group (quote emacspeak) :prefix "emacspeak-eterm-")

(autoload (quote emacspeak-eterm-remote-term) "emacspeak-eterm" "\
Start a terminal-emulator in a new buffer." t nil)

;;;***

;;;### (autoloads (emacspeak-filtertext) "emacspeak-filtertext" "emacspeak-filtertext.el"
;;;;;;  (15893 49008))
;;; Generated autoloads from emacspeak-filtertext.el

(autoload (quote emacspeak-filtertext) "emacspeak-filtertext" "\
Copy over text in region to special filtertext buffer in
preparation for interactively filtering text. " t nil)

;;;***

;;;### (autoloads (emacspeak-forms-find-file) "emacspeak-forms" "emacspeak-forms.el"
;;;;;;  (15893 49008))
;;; Generated autoloads from emacspeak-forms.el

(autoload (quote emacspeak-forms-find-file) "emacspeak-forms" "\
Visit a forms file" t nil)

;;;***

;;;### (autoloads (emacspeak-freeamp emacspeak-freeamp-freeamp-call-command
;;;;;;  emacspeak-freeamp-freeamp-command) "emacspeak-freeamp" "emacspeak-freeamp.el"
;;;;;;  (15892 55431))
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
;;;;;;  (15893 49008))
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
;;;;;;  (15893 49008))
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
;;;;;;  (15893 49008))
;;; Generated autoloads from emacspeak-imcom.el

(defgroup emacspeak-imcom nil "Jabber access from the Emacspeak audio desktop.")

(autoload (quote emacspeak-imcom) "emacspeak-imcom" "\
Start IMCom." t nil)

;;;***

;;;### (autoloads (emacspeak-m-player) "emacspeak-m-player" "emacspeak-m-player.el"
;;;;;;  (15893 49008))
;;; Generated autoloads from emacspeak-m-player.el

(defgroup emacspeak-m-player nil "Emacspeak media player settings.")

(autoload (quote emacspeak-m-player) "emacspeak-m-player" "\
Play specified resource using m-player.
Resource is an  MP3 file or m3u playlist.
The player is placed in a buffer in emacspeak-m-player-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-ocr) "emacspeak-ocr" "emacspeak-ocr.el"
;;;;;;  (15893 49008))
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

;;;### (autoloads (emacspeak-realaudio-browse emacspeak-realaudio
;;;;;;  emacspeak-realaudio-play-url-at-point emacspeak-realaudio-play)
;;;;;;  "emacspeak-realaudio" "emacspeak-realaudio.el" (15893 49008))
;;; Generated autoloads from emacspeak-realaudio.el

(autoload (quote emacspeak-realaudio-play) "emacspeak-realaudio" "\
Play a realaudio stream.  Uses files from your Realaudio
shortcuts directory for completion.  See documentation for
user configurable variable
emacspeak-realaudio-shortcuts-directory. " t nil)

(autoload (quote emacspeak-realaudio-play-url-at-point) "emacspeak-realaudio" "\
Play url under point as realaudio" t nil)

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
;;;;;;  "emacspeak-remote.el" (15893 49008))
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
;;;;;;  "emacspeak-rss.el" (15893 49008))
;;; Generated autoloads from emacspeak-rss.el

(defgroup emacspeak-rss nil "RSS Feeds for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-rss-display) "emacspeak-rss" "\
Retrieve and display RSS news feed." t nil)

(autoload (quote emacspeak-rss-browse) "emacspeak-rss" "\
Browse specified RSS feed." t nil)

;;;***

;;;### (autoloads (emacspeak-set-auditory-icon-player emacspeak-toggle-auditory-icons
;;;;;;  emacspeak-sounds-select-theme emacspeak-sounds-default-theme)
;;;;;;  "emacspeak-sounds" "emacspeak-sounds.el" (15893 49008))
;;; Generated autoloads from emacspeak-sounds.el

(defvar emacspeak-sounds-default-theme (expand-file-name "default-8k/" emacspeak-sounds-directory) "\
Default theme for auditory icons. ")

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
;;;;;;  "emacspeak-speak.el" (15893 49388))
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
;;;;;;  (15893 49008))
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
;;;;;;  "emacspeak-tabulate.el" (15893 49008))
;;; Generated autoloads from emacspeak-tabulate.el

(autoload (quote emacspeak-tabulate-region) "emacspeak-tabulate" "\
Voicifies the white-space of a table if one found.  Optional interactive prefix
arg mark-fields specifies if the header row information is used to mark fields
in the white-space." t nil)

;;;***

;;;### (autoloads (emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry"
;;;;;;  "emacspeak-tapestry.el" (15893 49008))
;;; Generated autoloads from emacspeak-tapestry.el

(autoload (quote emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry" "\
Describe the current layout of visible buffers in current frame." t nil)

;;;***

;;;### (autoloads (emacspeak-url-template-fetch emacspeak-url-template-load)
;;;;;;  "emacspeak-url-template" "emacspeak-url-template.el" (15893
;;;;;;  49008))
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

;;;### (autoloads (emacspeak-websearch-usenet emacspeak-websearch-emacspeak-archive
;;;;;;  emacspeak-websearch-dispatch) "emacspeak-websearch" "emacspeak-websearch.el"
;;;;;;  (15893 49008))
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

(autoload (quote emacspeak-websearch-usenet) "emacspeak-websearch" "\
Prompt and browse a Usenet newsgroup.
Optional interactive prefix arg results in prompting for a search term." t nil)

;;;***

;;;### (autoloads (emacspeak-xml-shell) "emacspeak-xml-shell" "emacspeak-xml-shell.el"
;;;;;;  (15893 49008))
;;; Generated autoloads from emacspeak-xml-shell.el

(defgroup emacspeak-xml-shell nil "XML browser for the Emacspeak desktop.")

(autoload (quote emacspeak-xml-shell) "emacspeak-xml-shell" "\
Start Xml-Shell on contents of system-id." t nil)

;;;***

;;;### (autoloads (turn-on-fast-voice-lock fast-voice-lock-mode)
;;;;;;  "fast-voice-lock" "fast-voice-lock.el" (15838 64633))
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

;;;### (autoloads (turn-on-jit-lock jit-voice-lock-mode) "jit-voice-lock"
;;;;;;  "jit-voice-lock.el" (15838 64633))
;;; Generated autoloads from jit-voice-lock.el

(autoload (quote jit-voice-lock-mode) "jit-voice-lock" "\
Toggle Just-in-time Lock mode.
With arg, turn Just-in-time Lock mode on if and only if arg is positive.
Enable it automatically by customizing group `voice-lock'.

When Just-in-time Lock mode is enabled, voiceification is different in the
following ways:

- Demand-driven buffer voiceification triggered by Emacs C code.
  This means initial voiceification of the whole buffer does not occur.
  Instead, voiceification occurs when necessary, such as when scrolling
  through the buffer would otherwise reveal unvoiceified areas.  This is
  useful if buffer voiceification is too slow for large buffers.

- Stealthy buffer voiceification if `jit-lock-stealth-time' is non-nil.
  This means remaining unvoiceified areas of buffers are voiceified if Emacs has
  been idle for `jit-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred voiceification.

- Deferred context voiceification if `jit-lock-defer-contextually' is
  non-nil.  This means voiceification updates the buffer corresponding to
  true syntactic context, after `jit-lock-stealth-time' seconds of Emacs
  idle time, while Emacs remains idle.  Otherwise, voiceification occurs
  on modified lines only, and subsequent lines can remain voiceified
  corresponding to previous syntactic contexts.  This is useful where
  strings or comments span lines.

Stealth voiceification only occurs while the system remains unloaded.
If the system load rises above `jit-lock-stealth-load' percent, stealth
voiceification is suspended.  Stealth voiceification intensity is controlled via
the variable `jit-lock-stealth-nice' and `jit-lock-stealth-lines'." t nil)

(autoload (quote turn-on-jit-lock) "jit-voice-lock" "\
Unconditionally turn on Just-in-time Lock mode." nil nil)

;;;***

;;;### (autoloads (turn-on-lazy-voice-lock lazy-voice-lock-mode)
;;;;;;  "lazy-voice-lock" "lazy-voice-lock.el" (15838 64633))
;;; Generated autoloads from lazy-voice-lock.el

(autoload (quote lazy-voice-lock-mode) "lazy-voice-lock" "\
Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive.  Enable it
automatically in your `~/.emacs' by:

 (setq voice-lock-support-mode 'lazy-voice-lock-mode)

When Lazy Lock mode is enabled, voiceification can be lazy in a number of ways:

 - Demand-driven buffer voiceification if `lazy-voice-lock-minimum-size' is non-nil.
   This means initial voiceification does not occur if the buffer is greater
   than `lazy-voice-lock-minimum-size' characters in length.  Instead, voiceification
   occurs when necessary, such as when scrolling through the buffer would
   otherwise reveal unvoiceified areas.  This is useful if buffer voiceification
   is too slow for large buffers.

 - Defer-driven buffer voiceification if `lazy-voice-lock-defer-driven' is non-nil.
   This means all voiceification is deferred, such as voiceification that occurs
   when scrolling through the buffer would otherwise reveal unvoiceified areas.
   Instead, these areas are seen momentarily unvoiceified.  This is useful if
   demand-driven voiceification is too slow to keep up with scrolling.

 - Deferred on-the-fly voiceification if `lazy-voice-lock-defer-time' is non-nil.
   This means on-the-fly voiceification does not occur as you type.  Instead,
   voiceification is deferred until after `lazy-voice-lock-defer-time' seconds of
   Emacs idle time, while Emacs remains idle.  This is useful if on-the-fly
   voiceification is too slow to keep up with your typing.

 - Stealthy buffer voiceification if `lazy-voice-lock-stealth-time' is non-nil.
   This means remaining unvoiceified areas of buffers are voiceified if Emacs has
   been idle for `lazy-voice-lock-stealth-time' seconds, while Emacs remains idle.
   This is useful if any buffer has demand- or defer-driven voiceification.

See also variables `lazy-voice-lock-stealth-lines', `lazy-voice-lock-stealth-nice' and
`lazy-voice-lock-stealth-verbose' for stealth voiceification.

Use \\[lazy-voice-lock-submit-bug-report] to send bug reports or feedback." t nil)

(autoload (quote turn-on-lazy-voice-lock) "lazy-voice-lock" "\
Unconditionally turn on Lazy Lock mode." nil nil)

;;;***

;;;### (autoloads (regexp-opt-depth regexp-opt) "regexp-opt" "regexp-opt.el"
;;;;;;  (15838 64633))
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

;;;### (autoloads (voice-lock-voiceify-buffer global-voice-lock-mode
;;;;;;  turn-on-voice-lock voice-lock-mode voice-lock-maximum-size
;;;;;;  voice-lock-maximum-decoration) "voice-lock" "voice-lock.el"
;;;;;;  (15838 64633))
;;; Generated autoloads from voice-lock.el

(defvar voice-lock-maximum-decoration nil "\
*Maximum decoration level for voiceification.
If nil, use the default decoration (typically the minimum available).
If t, use the maximum decoration available.
If a number, use that level of decoration (or if not available the maximum).
If a list, each element should be a cons pair of the form (MAJOR-MODE . LEVEL),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . t) (c++-mode . 2) (t . 1))
means use the maximum decoration available for buffers in C mode, level 2
decoration for buffers in C++ mode, and level 1 decoration otherwise.")

(defvar voice-lock-maximum-size (* 250 1024) "\
*Maximum size of a buffer for buffer voiceification.
Only buffers less than this can be voiceified when Voice Lock mode is turned on.
If nil, means size is irrelevant.
If a list, each element should be a cons pair of the form (MAJOR-MODE . SIZE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . 256000) (c++-mode . 256000) (rmail-mode . 1048576))
means that the maximum size is 250K for buffers in C or C++ modes, one megabyte
for buffers in Rmail mode, and size is irrelevant otherwise.")

(defvar voice-lock-mode-hook nil "\
Function or functions to run on entry to Voice Lock mode.")

(autoload (quote voice-lock-mode) "voice-lock" "\
Toggle Voice Lock mode.
With arg, turn Voice Lock mode on if and only if arg is positive.

When Voice Lock mode is enabled, text is voiceified as you type it:

 - Comments are displayed in `voice-lock-comment-personality';
 - Strings are displayed in `voice-lock-string-personality';
 - Certain other expressions are displayed in other personalities according to the
   value of the variable `voice-lock-keywords'.

You can enable Voice Lock mode in any major mode automatically by turning on in
the major mode's hook.  For example, put in your ~/.emacs:

 (add-hook 'c-mode-hook 'turn-on-voice-lock)

Alternatively, you can use Global Voice Lock mode to automagically turn on Voice
Lock mode in buffers whose major mode supports it and whose major mode is one
of `voice-lock-global-modes'.  For example, put in your ~/.emacs:

 (global-voice-lock-mode t)

There are a number of support modes that may be used to speed up Voice Lock mode
in various ways, specified via the variable `voice-lock-support-mode'.  Where
major modes support different levels of voiceification, you can use the variable
`voice-lock-maximum-decoration' to specify which level you generally prefer.
When you turn Voice Lock mode on/off the buffer is voiceified/devoiceified, though
voiceification occurs only if the buffer is less than `voice-lock-maximum-size'.

For example, to specify that Voice Lock mode use use Lazy Lock mode as a support
mode and use maximum levels of voiceification, put in your ~/.emacs:

 (setq voice-lock-support-mode 'lazy-voice-lock-mode)
 (setq voice-lock-maximum-decoration t)

To voiceify a buffer, without turning on Voice Lock mode and regardless of buffer
size, you can use \\[voice-lock-voiceify-buffer].

To voiceify a block (the function or paragraph containing point, or a number of
lines around point), perhaps because modification on the current line caused
syntactic change on other lines, you can use \\[voice-lock-voiceify-block].

The default Voice Lock mode personalities and their attributes are defined in the
variable `voice-lock-personality-attributes', and Voice Lock mode default settings in
the variable `voice-lock-defaults-alist'.  You can set your own default settings
for some mode, by setting a buffer local value for `voice-lock-defaults', via
its mode hook." t nil)

(autoload (quote turn-on-voice-lock) "voice-lock" "\
Turn on Voice Lock mode conditionally.
Turn on only if the device can display it." nil nil)

(defvar voice-lock-global-modes t "\
*Modes for which Voice Lock mode is automagically turned on.
Global Voice Lock mode is controlled by the `global-voice-lock-mode' command.
If nil, means no modes have Voice Lock mode automatically turned on.
If t, all modes that support Voice Lock mode have it automatically turned on.
If a list, it should be a list of `major-mode' symbol names for which Voice Lock
mode should be automatically turned on.  The sense of the list is negated if it
begins with `not'.  For example:
 (c-mode c++-mode)
means that Voice Lock mode is turned on for buffers in C and C++ modes only.")

(autoload (quote global-voice-lock-mode) "voice-lock" "\
Toggle Global Voice Lock mode.
With prefix ARG, turn Global Voice Lock mode on if and only if ARG is positive.
Displays a message saying whether the mode is on or off if MESSAGE is non-nil.
Returns the new status of Global Voice Lock mode (non-nil means on).

When Global Voice Lock mode is enabled, Voice Lock mode is automagically
turned on in a buffer if its major mode is one of `voice-lock-global-modes'." t nil)

(defvar voice-lock-support-mode nil "\
*Support mode for Voice Lock mode.
Support modes speed up Voice Lock mode by being choosy about when voiceification
occurs.  Known support modes are Fast Lock mode (symbol `fast-voice-lock-mode') and
Lazy Lock mode (symbol `lazy-voice-lock-mode').  See those modes for more info.
If nil, means support for Voice Lock mode is never performed.
If a symbol, use that support mode.
If a list, each element should be of the form (MAJOR-MODE . SUPPORT-MODE),
where MAJOR-MODE is a symbol or t (meaning the default).  For example:
 ((c-mode . fast-voice-lock-mode) (c++-mode . fast-voice-lock-mode) (t . lazy-voice-lock-mode))
means that Fast Lock mode is used to support Voice Lock mode for buffers in C or
C++ modes, and Lazy Lock mode is used to support Voice Lock mode otherwise.

The value of this variable is used when Voice Lock mode is turned on.")

(autoload (quote voice-lock-voiceify-buffer) "voice-lock" "\
Voiceify the current buffer the way `voice-lock-mode' would." t nil)

;;;***

;;;### (autoloads (xml-reformat-tags insert-xml read-xml) "xml-parse"
;;;;;;  "xml-parse.el" (15838 64633))
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

;;;### (autoloads (cd-tool) "cd-tool" "cd-tool.el" (15893 49008))
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
