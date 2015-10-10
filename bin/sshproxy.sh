#!/bin/bash
# Thanks http://forums.freebsd.org/showpost.php?p=110006&postcount=2

# $1 is the proxy server
# $2 is the port on the proxy
# $3 is the remote host to connect to
# $4 is the remote port to connect to

# Usage:
# Add to ssh config:
# ProxyCommand sshproxy.sh proxy 22 %h %p

if [[ $# < 3 || $# > 4 ]]; then
    echo "Usage: sshproxy.sh proxy proxyport host hostport"
    exit 0
fi

if [ -z "$4" ]; then
    remoteport=22
else
    remoteport=$4
fi

ssh -q -p $2 $1 "nc $3 $remoteport"
