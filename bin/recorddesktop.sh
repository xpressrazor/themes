#!/bin/bash
# Example recorddesktop.sh output.avi

STR="@"
FILENAME="out.avi"

if [ $STR ]; then
	FILENAME="$@"
else
	echo "No file name specified, using out.avi"
	FILENAME="out.avi"
fi

#FILENAME="$(echo $FILENAME | sed 's/\ /\\ /g')";

echo "Recording into $FILENAME ...";

ffmpeg -f alsa -i default -f x11grab -s $(xdpyinfo | grep 'dimensions:'|awk '{print $2}') -r 30 -i :0.0 -qscale 0 "$FILENAME"
#ffmpeg -f alsa -i default -f x11grab -s 1366x768 -r 30 -i :0.0 -sameq "$FILENAME"

