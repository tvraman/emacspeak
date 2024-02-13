#!/bin/bash
# Upload all .ogg files in cwd to the pulse server:

for i in *.ogg
do 
    pactl upload-sample `pwd`/$i `basename $i .ogg`
done
