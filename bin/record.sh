#!/bin/bash
 
if [ "$1" = --help ]
then
    echo "Usage: $0 [-m] [OUTPUT_FILE]"
    echo "Casts the screen to the given file. -m turns on the microphone."
fi
 
if [ "$1" = -m ]
then
    MIC=true
    shift
else
    MIC=false
fi
 
OUTFILE="$1"
 
if [ x = "x$OUTFILE" ]
then
    OUTFILE=screencast_`date +%Y-%m-%d_%H.%M.%S`.mp4
fi
 
COMBINED_SINK_NAME="combined"
SOURCE_MIC=$(LANG=C pactl stat | sed -n -e "s/Default Source: \(.*\)/\1/p")
SOURCE_OUTPUT=$(LANG=C pactl list | grep -A2 '^Source #' | grep 'Name: .*\.monitor$' | awk '{print $NF}' | tail -n1)
 
module_combine_sources() {
    echo "Seting up null sink"
    LOADED_MODULES+=($(LANG=C pactl load-module module-null-sink \
        sink_name="$COMBINED_SINK_NAME"))
    echo "Connecting VoIP source to null sink"
    LOADED_MODULES+=($(LANG=C pactl load-module module-loopback \
        source_dont_move=true sink_dont_move=true \
        sink="$COMBINED_SINK_NAME" source="$SOURCE_OUTPUT"))
    if $MIC
    then
        echo "Connecting Default source to null sink"
        LOADED_MODULES+=($(LANG=C pactl load-module module-loopback \
            source_dont_move=true sink_dont_move=true \
            sink="$COMBINED_SINK_NAME" source="$SOURCE_MIC"))
    fi
}
 
unload_modules() {
    set +e
    for each in ${LOADED_MODULES[@]}; do
        pactl unload-module $each
    done
}
OUTRES="1366x768"
FPS="20" # target FPS for x11grab
QUAL="medium"  # one of the many FFMPEG preset
 
trap unload_modules EXIT
module_combine_sources
avconv \
 -f x11grab -s 1366x768  -r "25" -i :0.0 \
 -f pulse -i "$COMBINED_SINK_NAME.monitor" \
 -vcodec libx264 -s $OUTRES -preset $QUAL -acodec libmp3lame -ar 44100 -threads 4 -qscale 3 -b 712000 \
 -bufsize 512k -f mp4 "$OUTFILE"
