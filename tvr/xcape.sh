#!/bin/sh

#XCape ==xcape -- reassign modifier keys etc.
TM=150 #timeout

#Shift_L gives open paren:
xcape -t $TM -e 'Shift_L=parenleft'

#Control by itself gives emacspeak modifier:
xcape -t $TM -e 'Control_L=Control_L|e'
