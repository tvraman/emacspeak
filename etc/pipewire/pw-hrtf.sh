#!/bin/sh
# Deprecated: The ZAM Plugins are not easy to build lately and there
#  are better solutions for spatial audio.
#  Zam Plugins from https://github.com/zamaudio/zam-plugins.git
# Azimuth:   Clockwise: -90..270 --- 0 is North.
# Elevation: -45 .. 90
# Depth: max 2.5
# Set up HRTF Virtual devices:

# Upper 4
pactl load-module module-ladspa-sink sink_name=FULeft plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=45,45,2.5 
pactl load-module module-ladspa-sink sink_name=RULeft plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=135,45,2.5 
pactl load-module module-ladspa-sink sink_name=RURight plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=225,45,2.5 
pactl load-module module-ladspa-sink sink_name=FURight plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=-45,45,2.5


# Lower 4
pactl load-module module-ladspa-sink sink_name=FLLeft plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=45,-45,2.5 
pactl load-module module-ladspa-sink sink_name=RLLeft plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=135,-45,2.5 
pactl load-module module-ladspa-sink sink_name=RLRight plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=225,-45,2.5 
pactl load-module module-ladspa-sink sink_name=FLRight plugin=ZamHeadX2-ladspa label=ZamHeadX2 control=-45,-45,2.5

