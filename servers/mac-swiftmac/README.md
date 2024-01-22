swiftmac
==============================================================================
This is a drop in replacement for the python "mac" server.

Quick Install (requires swift compiler)
------------------------------------------------------------------------------
 - make swiftmac (note: emits warnings)
 - Change the server in your configuration to "swiftmac"
 - Restart emacs

Recommended Settings
------------------------------------------------------------------------------
```
  (setopt mac-ignore-accessibility 't)
  (setopt mac-default-speech-rate 250)
  (setopt dtk-program "swiftmac")
  (defvar emacspeak-auditory-icon-function #'emacspeak-serve-auditory-icon)
  (setenv "SWIFTMAC_TONE_VOLUME" "0.5")
  (setenv "SWIFTMAC_SOUND_VOLUME" "0.5")
  (setenv "SWIFTMAC_VOICE_VOLUME" "1.0")
  (require 'emacspeak-setup)
```

Bugs?
------------------------------------------------------------------------------
 - https://github.com/robertmeta/swiftmac/issues
