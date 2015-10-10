#!/bin/bash

# Run when a openvpn connection is established.

IPTABLES="/sbin/iptables"
IP6TABLES="/sbin/ip6tables"

# Allow DNS
$IPTABLES -A OUTPUT -p udp --sport 1024:65535 --dport 53 -m state --state NEW,ESTABLISHED -j ACCEPT

# Allow openvpn
$IPTABLES -A OUTPUT -p udp --sport 1024:65535 --dport 1194 -m state --state NEW,ESTABLISHED -j ACCEPT

$IP6TABLES -P OUTPUT DROP
$IPTABLES -P OUTPUT DROP
