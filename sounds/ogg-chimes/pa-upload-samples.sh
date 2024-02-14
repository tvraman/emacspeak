#!/bin/bash
# Upload all .ogg files in cwd to the pulse server:
cd $(dirname $0)

for i in *.ogg
do 
    pactl upload-sample `pwd`/$i `basename $i .ogg`
done
