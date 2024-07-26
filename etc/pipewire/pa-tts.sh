#!/bin/sh
#Create our tts_mono_left and tts_mono_right  for pulse
# This is needed for TTS engines linked with Pulse, e.g. espeak, software-dtk

# A tts_mono_right and tts_mono_left device
# See https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/User/Modules/#module-remap-sink
# Emacspeak will use the first as the notification device.
pactl load-module module-remap-sink sink_name=tts_mono_right  master=binaural channels=2 master_channel_map=front-right,front-right, channel_map=front-left,front-right remix=no
pactl load-module module-remap-sink sink_name=tts_mono_left  master=binaural channels=2 master_channel_map=front-left,front-left, channel_map=front-left,front-right remix=no

# Above can likely be done with pw-link:
#https://www.thegeekdiary.com/pw-link-command-examples-in-linux/
# device: snoop -- to record and play (AsTeR)
pactl load-module module-combine-sink sink_name=snoop  sink_properties=device.description="Record-and-Play"

# to undo, use pactl unload-module <module name>
pactl upload-sample  /usr/share/sounds/freedesktop/stereo/bell.oga bell-window-system
