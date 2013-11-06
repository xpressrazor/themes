#!/bin/bash
# Example recordaudio.sh output.wav

STR="@" | tr -d ' '

FILENAME="out.wav"

if [ $STR ]; then
	FILENAME="$@"
	echo "$FILENAME is the filename"
fi

echo "Recording into $FILENAME ...";

ffmpeg -f alsa -i pulse "$FILENAME"

