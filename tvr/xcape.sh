#!/bin/sh

#XCape ==xcape -- reassign modifier keys etc.
#Control by itself gives emacspeak modifier.
#Shift_L gives open paren
#Shift_R gives Escape

TM=250 #timeout in ms for keyup

KEYS="Shift_L=Escape;\
Shift_R=Escape;\
Control_R=Control_L|e;\
Control_L=Control_L|e"
pidof xcape && kill -9 `pidof xcape`
xcape -t $TM -e  "$KEYS" 2>&1 > /dev/null &
