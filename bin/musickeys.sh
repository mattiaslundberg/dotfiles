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
   *)
       echo "Usage: $0 play|next|prev"
       exit 1
        ;;
esac
exit 0
