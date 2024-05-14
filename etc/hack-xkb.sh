#!/bin/bash
#beware: didn't work with xcape
#keeping it here for the future.
# Taken from http://www.emacs.dyerdwelling.family/emacs/20240309130457-emacs--kmonad-sway-kbd-map-locking/
DST=$HOME/.config/keymaps

# Reset keyboard layout (to your preferred language) 
setxkbmap us

# Apply sticky modifiers to a file 
xkbcomp $DISPLAY -xkb - |  \
    sed  's|SetMods|LatchMods|g'  >  \
         $DST/keymap_with_sticky_modifiers.xkb

# Reset keyboard layout (to your preferred language) 
setxkbmap us

# Apply locked modifiers to a file 
xkbcomp $DISPLAY -xkb - |  \
    sed  's|SetMods|LatchMods|g'  |  \
    sed  's|,clearLocks);|,clearLocks,latchToLock);|g'  >  \
         $DST/keymap_with_locked_modifiers.xkb
