swiftmac
==============================================================================
This is a server for MacOS. 

Quick Install (requires swift compiler)
------------------------------------------------------------------------------
 - make swiftmac (from emacspeak root, ignore warnings)
 - Change the server in your configuration to "swiftmac"
 - Start emacs

Recommended Settings
------------------------------------------------------------------------------
```
  ; stops doubletalk (when supported)
  (setopt mac-ignore-accessibility 't)

  ; use swiftmac 
  (setopt dtk-program "swiftmac")

  ; (optional) these are between 0 and 1
  (setenv "SWIFTMAC_TONE_VOLUME" "0.6")
  (setenv "SWIFTMAC_SOUND_VOLUME" "0.3")
  (setenv "SWIFTMAC_VOICE_VOLUME" "1.0")

  ; right or left, if left out will do both
  (setopt tts-notification-device "right")

  (add-to-list 'load-path "path/to/emacspeak/lisp")
  (require 'emacspeak-setup)

  ; (opiontal) do "en-US" or just ":Alex"
  (dtk-set-language "en-US:Alex")

  ; (optional) 0.6 is 60% in VoiceOver terms
  (dtk-set-rate 0.6 t)
```

Dependencies 
------------------------------------------------------------------------------
 - https://github.com/arkasas/OggDecoder.git


Bugs?
------------------------------------------------------------------------------
 - https://github.com/robertmeta/swiftmac/issues
