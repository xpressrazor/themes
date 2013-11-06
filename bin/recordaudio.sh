#!/bin/bash
# Example recorddesktop.sh output.avi

STR="@" | tr -d ' '

FILENAME="out.wav"

if [ $STR ]; then
	FILENAME="$@"
	echo "$FILENAME is the filename"
fi

echo "Recording into $FILENAME ...";

ffmpeg -f alsa -i pulse "$FILENAME"

