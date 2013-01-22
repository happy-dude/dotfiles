#!/bin/sh
#
# Test your ipv6 firewall rule set using:
# http://ipv6.chappell-family.com/ipv6tcptest/index.php
# Thank you Tim for providing this test tool.
#
# Ver. 2.0 (RHO and Logging, speciall ICMP Blocking)
# 29.12.2012
#
# From http://www.sixxs.net/wiki/IPv6_Firewalling

# Definitions
IP6TABLES='/usr/sbin/ip6tables'

# change LAN and IPv6 WAN interface name according your requirements
# WAN_IF='sixxs'
# LAN_IF='eth0'
# WLAN_IF='wlan0'

# SUBNETPREFIX='<subnet-prefix::>/48'
# MYTUNNEL='Your IP'
# SIXXSTUNNEL='Pop IP'

# First Flush and delete all:
$IP6TABLES -F INPUT
$IP6TABLES -F OUTPUT
$IP6TABLES -F FORWARD

$IP6TABLES -F
$IP6TABLES -X

# DROP all incomming traffic
$IP6TABLES -P INPUT DROP
$IP6TABLES -P OUTPUT DROP
$IP6TABLES -P FORWARD DROP

# Filter all packets that have RH0 headers:
$IP6TABLES -A INPUT -m rt --rt-type 0 -j DROP
$IP6TABLES -A FORWARD -m rt --rt-type 0 -j DROP
$IP6TABLES -A OUTPUT -m rt --rt-type 0 -j DROP

# Allow anything on the local link
$IP6TABLES -A INPUT  -i lo -j ACCEPT
$IP6TABLES -A OUTPUT -o lo -j ACCEPT

# Allow anything out on the internet
$IP6TABLES -A OUTPUT -j ACCEPT
# Allow established, related packets back in
$IP6TABLES -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT


# Allow the localnet access us:
$IP6TABLES -A INPUT    -j ACCEPT
$IP6TABLES -A OUTPUT   -j ACCEPT

# Allow Link-Local addresses
$IP6TABLES -A INPUT -s fe80::/10 -j ACCEPT
$IP6TABLES -A OUTPUT -s fe80::/10 -j ACCEPT

# Allow multicast
$IP6TABLES -A INPUT -d ff00::/8 -j ACCEPT
$IP6TABLES -A OUTPUT -d ff00::/8 -j ACCEPT

# Paranoia setting on ipv6 interface
$IP6TABLES -I INPUT -p tcp --syn -j DROP
$IP6TABLES -I FORWARD -p tcp --syn -j DROP
$IP6TABLES -I INPUT -p udp  -j DROP
$IP6TABLES -I FORWARD -p udp  -j DROP

# Allow forwarding on ipv6 interface
#$IP6TABLES -A FORWARD -m state --state NEW -s $SUBNETPREFIX -j ACCEPT
$IP6TABLES -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT

# Allow dedicated  ICMPv6 packettypes, do this in an extra chain because we need it everywhere
$IP6TABLES -N AllowICMPs
# Destination unreachable
$IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 1 -j ACCEPT
# Packet too big
$IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 2 -j ACCEPT
# Time exceeded
$IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 3 -j ACCEPT
# Parameter problem
$IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 4 -j ACCEPT
# Echo Request (protect against flood)
$IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 128 -m limit --limit 5/sec --limit-burst 10 -j ACCEPT
# Echo Reply
$IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 129 -j ACCEPT
#
# Only the sixxs POP is allowed to ping us (read FAQ this is a requirement)
#
#$IP6TABLES -A INPUT -p icmpv6 -s $SIXXSTUNNEL -d $MYTUNNEL  -j allowICMPs


# Log
#$IP6TABLES -A INPUT -j LOG --log-prefix "INPUT-v6:"
#$IP6TABLES -A FORWARD -j LOG  --log-prefix "FORWARD-v6:"
#$IP6TABLES -A OUTPUT -j LOG  --log-prefix "OUTPUT-v6:"
