#!/bin/sh
#
# Test your ipv6 firewall rule set using:
# http://ipv6.chappell-family.com/ipv6tcptest/index.php
# Thank you Tim for providing this test tool.
#
# Ver. 2.0 (RHO and Logging, speciall ICMPv6 Blocking)
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

# Reset all iptable chains, targets, modules, and tables
$IP6TABLES -P INPUT ACCEPT
$IP6TABLES -P FORWARD ACCEPT
$IP6TABLES -P OUTPUT ACCEPT
$IP6TABLES -F
$IP6TABLES -X

# Accept in/out from loopback
$IP6TABLES -A INPUT -i lo -m conntrack --ctstate NEW -j ACCEPT
$IP6TABLES -A INPUT -i lo -j ACCEPT
$IP6TABLES -A OUTPUT -o lo -m conntrack --ctstate NEW -j ACCEPT
$IP6TABLES -A OUTPUT -o lo -j ACCEPT

# Allow connections with a valid state
$IP6TABLES -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT      # Allow connections that we established
$IP6TABLES -A OUTPUT -m conntrack --ctstate NEW,RELATED,ESTABLISHED -j ACCEPT # and potentially new (but already permitted)
$IP6TABLES -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT    # Forward all legitimate response to forwarded traffic
$IP6TABLES -A FORWARD -o ppp0 -j ACCEPT                               # Forward point-to-point connections
$IP6TABLES -A FORWARD -i ppp0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT    # Allow new/already established point-to-point connections

# Allow certain ICMPv6 packets
$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type echo-reply -j ACCEPT   # ICMPv6 Echo Reply inbound
$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type destination-unreachable -j ACCEPT   # ICMPv6 Destination Unreachable inbound
$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type echo-request -m limit --limit 1/sec -j ACCEPT    # ICMPv6 Echo Request inbound
#$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type echo-request -j ACCEPT   # ICMPv6 Echo Request inbound
$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type time-exceeded -j ACCEPT   # ICMPv6 Time Exceeded inbound
$IP6TABLES -A OUTPUT -p icmpv6 -m icmpv6 --icmpv6-type echo-request -j ACCEPT   # ICMPv6 Echo Request outbound
$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type packet-too-big -j ACCEPT  # ICMPv6 Packet too big inbound
$IP6TABLES -A OUTPUT -p icmpv6 -m icmpv6 --icmpv6-type packet-too-big -j ACCEPT # ICMPv6 Packet too big outbound
$IP6TABLES -A INPUT -p icmpv6 -m icmpv6 --icmpv6-type parameter-problem -j ACCEPT   # ICMPv6 Parameter problem inbound
$IP6TABLES -A OUTPUT -p icmpv6 -m icmpv6 --icmpv6-type parameter-problem -j ACCEPT  # ICMPv6 Parameter problem outbound

# Allow Link-Local addresses
$IP6TABLES -A INPUT -s fe80::/10 -j ACCEPT
$IP6TABLES -A OUTPUT -s fe80::/10 -j ACCEPT

# Allow multicast
$IP6TABLES -A INPUT -d ff00::/8 -j ACCEPT
$IP6TABLES -A OUTPUT -d ff00::/8 -j ACCEPT

# Filter all packets that have RH0 headers:
$IP6TABLES -A INPUT -m rt --rt-type 0 -j DROP
$IP6TABLES -A FORWARD -m rt --rt-type 0 -j DROP
$IP6TABLES -A OUTPUT -m rt --rt-type 0 -j DROP

# TCP Filter
$IP6TABLES -N tcpfilter
$IP6TABLES -A INPUT -p tcp -j tcpfilter
$IP6TABLES -A tcpfilter -p tcp -m tcp ! --tcp-flags FIN,SYN,RST,ACK SYN -m conntrack --ctstate NEW -j DROP
$IP6TABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG FIN,PSH,URG -j DROP
$IP6TABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG FIN,SYN,RST,PSH,ACK,URG -j DROP
$IP6TABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG NONE -j DROP
$IP6TABLES -A tcpfilter -p tcp -m tcp --tcp-flags SYN,RST SYN,RST -j DROP
$IP6TABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN FIN,SYN -j DROP

# Filter traffic to UDP port 0 (reserved; TCP has no port 0...) and port 1 (TCP Port Service Multiplexer - TCPMUX)
$IP6TABLES -A INPUT -p tcp -m tcp --tcp-flags FIN,SYN,RST,ACK SYN -j DROP
$IP6TABLES -A INPUT -p tcp -m tcp --dport 0 -j DROP
$IP6TABLES -A INPUT -p udp -m udp --dport 0 -j DROP
$IP6TABLES -A INPUT -p tcp -m tcp --dport 1 -j DROP
$IP6TABLES -A INPUT -p udp -m udp --dport 1 -j DROP

$IP6TABLES -A INPUT -j DROP
$IP6TABLES -A OUTPUT -j DROP
$IP6TABLES -A FORWARD -j DROP

# # Allow dedicated  ICMPv6 packettypes, do this in an extra chain because we need it everywhere
# $IP6TABLES -N AllowICMPs
# # Destination unreachable
# $IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 1 -j ACCEPT
# # Packet too big
# $IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 2 -j ACCEPT
# # Time exceeded
# $IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 3 -j ACCEPT
# # Parameter problem
# $IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 4 -j ACCEPT
# # Echo Request (protect against flood)
# $IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 128 -m limit --limit 5/sec --limit-burst 10 -j ACCEPT
# # Echo Reply
# $IP6TABLES -A AllowICMPs -p icmpv6 --icmpv6-type 129 -j ACCEPT
#
# Only the sixxs POP is allowed to ping us (read FAQ this is a requirement)
#
#$IP6TABLES -A INPUT -p icmpv6 -s $SIXXSTUNNEL -d $MYTUNNEL  -j allowICMPs


# Log
#$IP6TABLES -A INPUT -j LOG --log-prefix "INPUT-v6:"
#$IP6TABLES -A FORWARD -j LOG  --log-prefix "FORWARD-v6:"
#$IP6TABLES -A OUTPUT -j LOG  --log-prefix "OUTPUT-v6:"
