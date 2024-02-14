#!/bin/bash
# Remove all .ogg files in cwd from the pulse server:
cd $(dirname $0)

for i in *.ogg 
do
    pactl remove-sample $(basename $i .ogg)
done
