#!/bin/bash
# Example ./torrent.sh Ubuntu Linux Or put it in PATH and type torrent.sh

STR="("

for i in "$@";		# for loop of all commandline parameters
	do 
		STR+="$i|" 	# add pipe character for egrep	
done

STR="${STR%?})"  # remove last pipe character
find ~/Torrent/ | egrep -i "$STR" # search (file1|file2|file3)
