#!/bin/sh

case $1 in
   "play")
       dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause
       ;;
   "next")
       dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next
       ;;
   "prev")
       dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous
       ;;
    "mute")
       if amixer -c 0 get Master | grep -q off ; then
           amixer set Master unmute
           amixer set Headphone unmute
           amixer set Speaker unmute
       else
           amixer set Master mute
       fi
   ;;
   *)
       echo "Usage: $0 play|next|prev|mute"
       exit 1
        ;;
esac
exit 0
