;;; Default theme as 44.1K stereo  -*- lexical-binding: t; -*-
(require 'emacspeak-sounds)
(emacspeak-sounds-define-theme
 (expand-file-name "classic/" emacspeak-sounds-directory)
 ".wav")
