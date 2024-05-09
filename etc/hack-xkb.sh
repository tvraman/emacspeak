#!/bin/bash
#beware: didn't work with xcape
#keeping it here for the future.
DST=$HOME/.config

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
