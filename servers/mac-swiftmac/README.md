swiftmac
==============================================================================
This is a drop in replacement for the python "mac" server.

Quick Install (requires swift compiler)
------------------------------------------------------------------------------
 - make (note: emits warnings)
 - Change the server in your configuration to "swiftmac"
 - Restart emacs

Recommended Settings
------------------------------------------------------------------------------
```
  (setq dtk-program "swiftmac")
  (setenv "SWIFTMAC_TONE_VOLUME" "0.5")
  (setenv "SWIFTMAC_SOUND_VOLUME" "0.5")
  (setenv "SWIFTMAC_VOICE_VOLUME" "1.0")
  (defvar emacspeak-auditory-icon-function #'emacspeak-serve-auditory-icon)
  (require 'emacspeak-setup)
  (dtk-set-rate 250 t)
```

Known Issues
------------------------------------------------------------------------------
 1. Stop is too aggressive (stops audio icons)
 2. Protocol "a" and "p" conflated, need to follow DTK model, a is queue and 
    p means play instantly
 4. Extremely long speaking blocks can choke server, implement internal chunking

Ignored Issues
------------------------------------------------------------------------------
Due to implementation choices, many things will not be fixed until swiftmac v2

 1. Effects (reverb, single-ear mode, differential volume, mono-mode)
 2. Beepcaps support

