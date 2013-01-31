#! /bin/bash

##############################################################################
#
### Author: Stanley Chan
### Version Saturday 12 January 2013 :: Complete ruleset
############	** Ran from IPTables.bash (31 Dec 2008) version and coverted with iptables-save; added and edited rules **
############			--Complete version: Full rules, with commented elements
############			--Active version:	Runs the same as the Complete version, but without the commented elements
#
## /etc/init.d/firewall
#
### Based on rules from:
### http://www.novell.com/coolsolutions/feature/18139.html
### http://www.linuxquestions.org/questions/linux-security-4/stealth-iptables-ruleset-21338/
### http://fixunix.com/security/17626-shields-up-reports-one-open-port-through-iptables.html
### http://www.dslreports.com/forum/r20642422-Help-Configuring-Router-IPTables-to-stealth-all-ports-
### http://www.remoteroot.net/category/firewall/
### http://www.remoteroot.net/2007/07/18/10/
### http://billauer.co.il/ipmasq-html.html
### http://ubuntuforums.org/showthread.php?t=159661
### https://www.linuxquestions.org/questions/linux-newbie-8/iptables-and-irc-clients.-158049/
### http://insidetrust.blogspot.com/2010/12/example-iptables-firewall-ruleset-for.html
#
##### BEGIN INIT INFO
## Provides: Firewall for Router/Modem/Switch [Westell Versalink 7500]
## Required-Start: $network syslog
## Required-Stop:
## Should-Stop:
## Default-Start: 3 4 5
## Default-Stop: 0 1 2 6
## Short-Description: Firewall Configuration
##### END INIT INFO
#
#
### Following no longer applied with 2011 version::
###		xx Modified script for Ubuntu build; lines with eth0 have been replaced with wlan1 to accomodate for wireless. xx
#
##############################################################################

# Definitions
IPTABLES='/usr/sbin/iptables'

# Reset all iptable chains, targets, modules, and tables
$IPTABLES -P INPUT ACCEPT
$IPTABLES -P FORWARD ACCEPT
$IPTABLES -P OUTPUT ACCEPT
$IPTABLES -F
$IPTABLES -t nat -F
$IPTABLES -X

# Accept in/out from loopback
$IPTABLES -A INPUT -i lo -m conntrack --ctstate NEW -j ACCEPT
$IPTABLES -A INPUT -i lo -j ACCEPT
$IPTABLES -A OUTPUT -o lo -m conntrack --ctstate NEW -j ACCEPT
$IPTABLES -A OUTPUT -o lo -j ACCEPT

# Allow connections with a valid state
$IPTABLES -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT      # Allow connections that we established
$IPTABLES -A OUTPUT -m conntrack --ctstate NEW,RELATED,ESTABLISHED -j ACCEPT # and potentially new (but already permitted)
$IPTABLES -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT    # Forward all legitimate response to forwarded traffic
$IPTABLES -A FORWARD -o ppp0 -j ACCEPT                               # Forward point-to-point connections
$IPTABLES -A FORWARD -i ppp0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT    # Allow new/already established point-to-point connections

# Allow certain ICMP packets
$IPTABLES -A INPUT -p icmp -m icmp --icmp-type 0 -j ACCEPT   # ICMP Echo Reply inbound
$IPTABLES -A INPUT -p icmp -m icmp --icmp-type 3 -j ACCEPT   # ICMP Destination Unreachable inbound
$IPTABLES -A INPUT -p icmp -m icmp --icmp-type 8 -m limit --limit 1/sec -j ACCEPT    # ICMP Echo Request inbound
#$IPTABLES -A INPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT   # ICMP Echo Request inbound
$IPTABLES -A INPUT -p icmp -m icmp --icmp-type 11 -j ACCEPT   # ICMP Time Exceeded inbound
$IPTABLES -A OUTPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT   # ICMP Echo Request outbound

# TCP Filter
$IPTABLES -N tcpfilter
$IPTABLES -A INPUT -p tcp -j tcpfilter
$IPTABLES -A tcpfilter -p tcp -m tcp ! --tcp-flags FIN,SYN,RST,ACK SYN -m conntrack --ctstate NEW -j DROP
$IPTABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG FIN,PSH,URG -j DROP
$IPTABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG FIN,SYN,RST,PSH,ACK,URG -j DROP
$IPTABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG NONE -j DROP
$IPTABLES -A tcpfilter -p tcp -m tcp --tcp-flags SYN,RST SYN,RST -j DROP
$IPTABLES -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN FIN,SYN -j DROP

# PREROUTING and POSTROUTING with NAT
$IPTABLES -t nat -A PREROUTING -d 10.0.0.1/32 -p tcp -m tcp --dport 25 -j DNAT --to-destination 192.168.1.254
$IPTABLES -t nat -A PREROUTING -d 10.0.0.1/32 -p tcp -m tcp --dport 80 -j DNAT --to-destination 192.168.10.253
$IPTABLES -t nat -A PREROUTING -d 10.0.0.1/32 -p tcp -m tcp --dport 110 -j DNAT --to-destination 192.168.10.254
$IPTABLES -t nat -A PREROUTING -d 10.0.0.1/32 -p tcp -m tcp --dport 444 -j DNAT --to-destination 192.168.10.254:443
$IPTABLES -t nat -A POSTROUTING -s 192.168.1.0/24 -j SNAT --to-source 10.0.0.1
$IPTABLES -t nat -A POSTROUTING -j MASQUERADE

# Filter IRC traffic
$IPTABLES -A INPUT -p tcp -m tcp --dport 194 -j DROP     # IRC
$IPTABLES -A INPUT -p udp -m udp --dport 194 -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --dport 529 -j DROP     # IRC-SERV
$IPTABLES -A INPUT -p udp -m udp --dport 529 -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --dport 994 -j DROP     # IRC over TLS/SSL
$IPTABLES -A INPUT -p udp -m udp --dport 994 -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --dport 6667 -j DROP    # IRC; many trojans/backdoors use this port
$IPTABLES -A INPUT -p udp -m udp --dport 6667 -j DROP

# Filter traffic to UDP port 0 (reserved; TCP has no port 0...) and port 1 (TCP Port Service Multiplexer - TCPMUX)
$IPTABLES -A INPUT -p tcp -m tcp --tcp-flags FIN,SYN,RST,ACK SYN -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --dport 0 -j DROP
$IPTABLES -A INPUT -p udp -m udp --dport 0 -j DROP
$IPTABLES -A INPUT -p tcp -m tcp --dport 1 -j DROP
$IPTABLES -A INPUT -p udp -m udp --dport 1 -j DROP

$IPTABLES -A INPUT -j DROP
$IPTABLES -A OUTPUT -j DROP
$IPTABLES -A FORWARD -j DROP
