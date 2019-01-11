#!/bin/sh

#XCape ==xcape -- reassign modifier keys etc.

#Shift_L gives open paren:
xcape -t 250 -e 'Shift_L=parenleft'

#Control by itself gives emacspeak modifier:
xcape -t 100 -e 'Control_L=Control_L|e'
