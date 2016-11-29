#!/bin/bash
play() {
  if [[ $1 == radio3 ]]; then
    playlist="http://open.live.bbc.co.uk/mediaselector/5/select/version/2.0/mediaset/http-icy-aac-lc-a/format/pls/vpid/bbc_radio_three.pls"
  else
    playlist="http://bbcmedia.ic.llnwd.net/stream/bbcmedia_$1_mf_p"
  fi
  echo $playlist
  if mpc
  then
    mpc add $playlist
    mpc play
  else
    mplayer $playlist
  fi
}
if [ -z "$1" ]; then
  echo "Select a station:"
  select s in radio1 radio3 radio3 radio4fm radio5live 6music
  do
    play ${s##* }
    break
  done
else
  play $1
fi
# GistID: 4052479
