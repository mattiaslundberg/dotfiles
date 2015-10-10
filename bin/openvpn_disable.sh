#!/bin/bash

# Run when a openvpn connection is established.

IPTABLES="/sbin/iptables"
IP6TABLES="/sbin/ip6tables"

$IP6TABLES -P OUTPUT ACCEPT

$IPTABLES -P OUTPUT ACCEPT
