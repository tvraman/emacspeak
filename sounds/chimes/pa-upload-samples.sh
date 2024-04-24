#!/bin/bash
# Upload all .ogg files in cwd to the pulse server:
cd $(dirname $0)
d=$(pwd)
for i in *.ogg
do 
    pactl upload-sample ${d}/$i $(basename $i .ogg)
done
