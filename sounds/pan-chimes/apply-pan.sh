#!/bin/sh
#This script is used to generate panned chimes.
#The only variation among icons is the frequency of the autopan effect.
#Usage pan filename frequency 

function pan () {
    mplayer -af ladspa=tap_autopan:tap_autopan:$2:100:0  -ao pcm:file=tmp.wav ../chimes/$1
    sox tmp.wav -b 16 -r 44100  $1
    \rm  tmp.wav
}

pan alarm.wav 3
pan alert-user.wav 1
pan ask-question.wav 2
pan ask-short-question.wav 1
pan button.wav 5
pan center.wav 1
pan close-object.wav 2
pan complete.wav 5
pan delete-object.wav 1 
pan deselect-object.wav 2 
pan ellipses.wav 2 
pan fill-object.wav 1 
pan help.wav 2 
pan item.wav 1 
pan large-movement.wav 3 
pan left.wav 1 
pan mark-object.wav 1
pan modified-object.wav 1
pan n-answer.wav 1
pan network-down.wav 1
pan network-up.wav 1
pan new-mail.wav 1
pan news.wav 1
pan no-answer.wav 1
pan off.wav 1
pan on.wav 1
pan open-object.wav 2
pan paragraph.wav 1
pan progress.wav 1
pan right.wav 1
pan save-object.wav 1
pan scroll.wav 1
pan search-hit.wav 1
pan search-miss.wav 1
pan section.wav 2
pan select-object.wav 1
pan shutdown.wav 1
pan task-done.wav 1
pan unmodified-object.wav 2
pan voice-mail.wav 1
pan warn-user.wav 3
pan window-resize.wav 1
pan working.wav 1
pan yank-object.wav 5
pan y-answer.wav 5
pan yes-answer.wav 5
