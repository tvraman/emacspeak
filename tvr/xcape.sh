#!/bin/sh

#XCape ==xcape -- reassign modifier keys etc.
#Control by itself gives emacspeak modifier.
#Shift_L gives open paren
#Shift_R gives Escape
# Control_R gives Multi_key (compose)
TM=150 #timeout in ms for keyup

KEYS="Shift_L=parenleft;\
Shift_R=Escape;\
Control_R=Multi_key;\
Control_L=Control_L|e"

xcape -t $TM -e  "$KEYS"
