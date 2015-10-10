#!/bin/bash

if [[ $# < 2 || $# > 4 ]]; then
	echo "Usage: remotecap.sh host interface [time [filter]]"
	exit 0
fi

if [ -z "$3" ]; then
	time=30
else
	time=$3
fi

if [ -z "$4" ]; then
	filter="not port 22"
else
	filter=$4
fi

timestamp=$(date +"%m-%d-%H_%M_%S")
filename="$1-$timestamp.pcap"

ssh $1 "mkdir /pcap &> /dev/null"
ssh $1 "tcpdump -i $2 -U -s0 -w /pcap/$filename \"$filter\"&> /dev/null &"

# Save the latest pid
ssh $1 "echo \`pidof tcpdump\` | awk '{ print \$1 }' > /pcap/${filename}.pid"

sleep $time

ssh $1 "kill \`cat /pcap/${filename}.pid\`"

mkdir /tmp/pcap/ &> /dev/null

scp -r $1:/pcap/$filename /tmp/pcap/$filename &> /dev/null

ssh $1 "rm /pcap/${filename}*"

wireshark /tmp/pcap/$filename &> /dev/null 
