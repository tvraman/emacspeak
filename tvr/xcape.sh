#!/bin/sh

#XCape ==xcape -- reassign modifier keys
#Control by itself gives emacspeak modifier.
# See forthcoming blog article for rationale.

TM=150 #timeout in ms for keyup

KEYS="\
Shift_L=Control_L|s;\
Shift_R=Control_L|r;\
Super_L=Control_L|c;\
Alt_L=Control_L|x;\
Alt_R=Control_L|x;\
Control_R=Control_L|e;\
Control_L=Control_L|e"
pidof xcape && kill -9 `pidof xcape`
xcape -t $TM -e  "$KEYS" 2>&1 > /dev/null 
