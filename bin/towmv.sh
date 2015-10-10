#!/bin/bash

if [[ $# != 1 ]]; then
	echo "usage towmv.sh filename"
	exit 1
fi

FILE_NOEXT=${1%.*}

ffmpeg -i $1 -y -copyts -c:v wmv2 -b:v 15360k -maxrate:v 15360k -bufsize:v 15360k -r 24000/1001 -g 15 -c:a wmav2 -b:a 112k -ac 2 -map 0:0 -map 0:1 -sn -f asf ${FILE_NOEXT}.wmv
