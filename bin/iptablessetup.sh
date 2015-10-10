#!/bin/bash

IPTABLES="/sbin/iptables"
IP6TABLES="/sbin/ip6tables"

# Helper function for confirming allow rules
confirm() {
	while true; do
		read -p "Allow $1? " yn
		case $yn in
			[Yy]* ) echo 1; break;;
			[Nn]* ) echo 0; break;;
		esac
	done
}

# Drop all IPv6
if [[ -x $IP6TABLES ]]; then
	echo "IPv6 support is present."
	$IP6TABLES -F INPUT
	$IP6TABLES -F OUTPUT
	$IP6TABLES -F FORWARD
	if [[ $(confirm "IPv6") == 1 ]]; then
		echo "Allow IPv6"
		IPV6SUPPORT=true
		$IP6TABLES -F
		$IP6TABLES -P INPUT ACCEPT
		$IP6TABLES -P OUTPUT ACCEPT
		$IP6TABLES -P FORWARD ACCEPT
	else
		echo "Block all IPv6"
		$IP6TABLES -F
		$IP6TABLES -P INPUT DROP
		$IP6TABLES -P OUTPUT DROP
		$IP6TABLES -P FORWARD DROP
	fi
fi

# Allow all traffic so current sessions won't die...
$IPTABLES -P INPUT ACCEPT
$IPTABLES -P FORWARD ACCEPT
$IPTABLES -P OUTPUT ACCEPT
$IPTABLES -F

# Allow established
$IPTABLES -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
if [[ $IPV6SUPPORT ]]; then
	$IP6TABLES -A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
fi

# Drop invalid
$IPTABLES -A INPUT -m state --state INVALID -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG NONE -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags SYN,FIN SYN,FIN -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags SYN,RST SYN,RST -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags FIN,RST FIN,RST -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags ACK,FIN FIN -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags ACK,URG URG -j DROP
if [[ $IPV6SUPPORT ]]; then
	$IP6TABLES -A INPUT -m state --state INVALID -j DROP
	$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG NONE -j DROP
	$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags SYN,FIN SYN,FIN -j DROP
	$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags SYN,RST SYN,RST -j DROP
	$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags FIN,RST FIN,RST -j DROP
	$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags ACK,FIN FIN -j DROP
	$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags ACK,URG URG -j DROP
fi

# WEB and DoS protection
if [[ $(confirm "WEB on port 80 and 443") == 1 ]]; then
	$IPTABLES -A INPUT -p tcp -m multiport --dports 80,443 -m state --state NEW,ESTABLISHED -m limit --limit 25/minute --limit-burst 100 -j ACCEPT
	if [[ $IPV6SUPPORT && $(confirm "for IPv6") == 1 ]]; then
		$IP6TABLES -A INPUT -p tcp -m multiport --dports 80,443 -m state --state NEW,ESTABLISHED -m limit --limit 25/minute --limit-burst 100 -j ACCEPT
	fi
	echo "Allowing WEB on 80 and 443"
fi

if [[ $(confirm "SSH server on port 22") == 1 ]]; then
	$IPTABLES -A INPUT -p tcp --dport 22 -m state --state NEW,ESTABLISHED -m limit --limit 10/minute --limit-burst 25 -j ACCEPT
	if [[ $IPV6SUPPORT && $(confirm "for IPv6") == 1 ]]; then
		$IP6TABLES -A INPUT -p tcp --dport 22 -m state --state NEW,ESTABLISHED -m limit --limit 10/minute --limit-burst 25 -j ACCEPT
	fi
	echo "Allowing SSHD on port 22"
fi
if [[ $(confirm "SSH server on port 2022") == 1 ]]; then
	$IPTABLES -A INPUT -p tcp --dport 2022 -m state --state NEW,ESTABLISHED -m limit --limit 10/minute --limit-burst 25 -j ACCEPT
	if [[ $IPV6SUPPORT && $(confirm "for IPv6") == 1 ]]; then
		$IP6TABLES -A INPUT -p tcp --dport 2022 -m state --state NEW,ESTABLISHED -m limit --limit 10/minute --limit-burst 25 -j ACCEPT
	fi
	echo "Allowing SSHD on port 2022"
fi

# Remote desktop
if [[ $(confirm "Incoming rdp desktop connections") == 1 ]]; then
	echo "Allowing RDP at 3389"
	$IPTABLES -A INPUT -m state --state NEW -m tcp -p tcp --dport 3389 -s 192.168.1.0/16 -j ACCEPT
fi

# Allow incoming ping
$IPTABLES -A INPUT -p icmp --icmp-type echo-request -j ACCEPT

# Allow all icmp for ipv6
if [[ $IPV6SUPPORT ]]; then
	$IP6TABLES -A INPUT -p icmpv6 -j ACCEPT
fi

# Allow Skype
if [[ $(confirm "Skype connections") == 1 ]]; then
	$IPTABLES -A INPUT -m state --state NEW -m udp -p udp --dport 26187 -j ACCEPT
	$IPTABLES -A INPUT -m state --state NEW -m tcp -p tcp --dport 26187 -j ACCEPT
fi

if [[ $(confirm "SIP connections") == 1 ]]; then
	$IPTABLES -A INPUT -m state --state NEW -m udp -p udp --dport 5060 -j ACCEPT
	$IPTABLES -A INPUT -m state --state NEW -m tcp -p tcp --dport 5060 -j ACCEPT
	$IPTABLES -A INPUT -m state --state NEW -m udp -p udp --dport 9078 -j ACCEPT
	$IPTABLES -A INPUT -m state --state NEW -m udp -p udp --dport 9078 -j ACCEPT
fi

# Chain defaults
$IPTABLES -P INPUT DROP
$IPTABLES -P FORWARD DROP
$IPTABLES -P OUTPUT ACCEPT

if [[ $IPV6SUPPORT ]]; then
	$IP6TABLES -P INPUT DROP
	$IP6TABLES -P FORWARD DROP
	$IP6TABLES -P OUTPUT ACCEPT
fi

# Allow loopback
$IPTABLES -A INPUT -i lo -j ACCEPT
if [[ $IPV6SUPPORT ]]; then
	$IP6TABLES -A INPUT -i lo -j ACCEPT
fi

# Logging
if [[ $(confirm "logging of dropped packets") == 1 ]]; then
	$IPTABLES -A INPUT -m limit --limit 2/min -j LOG --log-prefix "Dropped: " --log-level 7
	if [[ $IPV6SUPPORT ]]; then
		$IPTABLES -A INPUT -m limit --limit 2/min -j LOG --log-prefix "Dropped (6): " --log-level 7
	fi
fi

# Automatically restore the rules upon reboots.
if [[ -d "/etc/iptables/" ]]; then
	echo "Saving rules for arch iptables versions"
	sudo sh -c "iptables-save > /etc/iptables/iptables.rules"
	sudo systemctl enable iptables.service
	if [[ $IPV6SUPPORT ]]; then
		sudo sh -c "ip6tables-save > /etc/iptables/ip6tables.rules"
		sudo systemctl enable ip6tables.service
	fi
else
	echo "Saving rules for deb iptables versions"
	sudo sh -c "iptables-save > /etc/iptables.rules"
	if [[ $IPV6SUPPORT ]]; then
		sudo sh -c "ip6tables-save > /etc/ip6tables.rules"
	fi
	
	# Enable loading of the rules since we don't have automatic support for it...
	sudo sh -c "echo \"#\!/bin/sh\" > /etc/network/if-pre-up.d/iptables"
	sudo sh -c "echo iptables-restore \< /etc/iptables.rules >> /etc/network/if-pre-up.d/iptables"
	if [[ $IPV6SUPPORT ]]; then
		sudo sh -c "echo ip6tables-restore \< /etc/ip6tables.rules >> /etc/network/if-pre-up.d/iptables"
	fi
	sudo sh -c "echo \"exit 0\" >> /etc/network/if-pre-up.d/iptables"
fi
