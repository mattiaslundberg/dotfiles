#!/bin/bash

# Run when a openvpn connection is established.

IPTABLES="/sbin/iptables"
IP6TABLES="/sbin/ip6tables"

$IP6TABLES -P OUTPUT DROP
$IPTABLES -P OUTPUT ACCEPT
