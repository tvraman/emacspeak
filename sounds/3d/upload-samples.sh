#!/bin/bash
# Upload Samples To Pulse Server

for i in *.ogg
         do
             pactl upload-sample $i `basename $i .ogg`
             done
