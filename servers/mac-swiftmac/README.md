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
  (setq mac-ignore-accessibility 't)
  (setq dtk-program "swiftmac")
  (setenv "SWIFTMAC_TONE_VOLUME" "0.5")
  (setenv "SWIFTMAC_SOUND_VOLUME" "0.5")
  (setenv "SWIFTMAC_VOICE_VOLUME" "1.0")
  (defvar emacspeak-auditory-icon-function #'emacspeak-serve-auditory-icon)
  (require 'emacspeak-setup)
  (dtk-set-rate 250 t)
```

Bugs?
------------------------------------------------------------------------------
 - https://github.com/robertmeta/swiftmac/issues
