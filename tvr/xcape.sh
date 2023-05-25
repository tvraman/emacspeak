#!/bin/sh

#XCape ==xcape -- reassign modifier keys
#Control by itself gives emacspeak modifier.
# See forthcoming blog article for rationale.
# Swapping C-x and C-c from what is described in the blog.

# Start with  timeout  250, then tune it down (suggest 10% every few
#days)
# Customize echo-keystrokes in Emacs: I have it as 0.001


TM=175 #timeout in ms for keyup

KEYS="\
Shift_L=Control_L|s;\
Shift_R=Control_L|r;\
Super_L=Control_L|x;\
Alt_L=Control_L|c;\
Alt_R=Control_L|c;\
Control_L=Control_L|e"
pidof xcape && kill -9 `pidof xcape`
xcape -t $TM -e  "$KEYS" 2>&1 > /dev/null 
