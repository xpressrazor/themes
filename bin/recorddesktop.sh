#!/bin/bash
# Example recorddesktop.sh output.avi

STR="@" | tr -d ' '
FILENAME="out.avi"

if [ $STR ]; then
	FILENAME="$@"
fi

echo "Recording into $FILENAME ...";

ffmpeg -f alsa -i default -f x11grab -s $(xdpyinfo | grep 'dimensions:'|awk '{print $2}') -r 30 -i :0.0 -qscale 0 "$FILENAME"


