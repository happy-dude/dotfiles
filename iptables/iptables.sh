#! /bin/bash

# Reset all iptable chains, targets, modules, and tables
iptables -P INPUT ACCEPT
iptables -P FORWARD ACCEPT
iptables -P OUTPUT ACCEPT
iptables -F
iptables -X

# Accept in/out from loopback
iptables -A INPUT -i lo -m conntrack --ctstate NEW -j ACCEPT
iptables -A INPUT -i lo -j ACCEPT
iptables -A OUTPUT -o lo -m conntrack --ctstate NEW -j ACCEPT
iptables -A OUTPUT -o lo -j ACCEPT

# Allow connections with a valid state
iptables -A INPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT      # Allow connections that we established
iptables -A OUTPUT -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT     # and potentially new (but already permitted)
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT    # Forward all legitimate response to forwarded traffic
iptables -A FORWARD -i eth0 -o ppp0 -j ACCEPT                               # Forward point-to-point connections
iptables -A FORWARD -i ppp0 -o eth0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT    # Allow new/already established point-to-point connections

# Allow certain ICMP packets
iptables -A INPUT -p icmp -m icmp --icmp-type 0 -j ACCEPT   # ICMP Echo Reply inbound
iptables -A INPUT -p icmp -m icmp --icmp-type 3 -j ACCEPT   # ICMP Destination Unreachable inbound
iptables -A INPUT -p icmp -m icmp --icmp-type 8 -m limit --limit 1/sec -j ACCEPT    # ICMP Echo Request inbound
#iptables -A INPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT   # ICMP Echo Request inbound
iptables -A INPUT -p icmp -m icmp --icmp-type 11 -j ACCEPT   # ICMP Time Exceeded inbound
iptables -A OUTPUT -p icmp -m icmp --icmp-type 8 -j ACCEPT   # ICMP Echo Request outbound

# Allow DHCP
iptables -A INPUT -i eth0 -p udp -m udp --sport 67 -j DROP

# TCP Filter
iptables -N tcpfilter
iptables -A INPUT -p tcp -j tcpfilter
iptables -A tcpfilter -p tcp -m tcp ! --tcp-flags FIN,SYN,RST,ACK SYN -m conntrack --ctstate NEW -j DROP
iptables -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG FIN,PSH,URG -j DROP
iptables -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG FIN,SYN,RST,PSH,ACK,URG -j DROP
iptables -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN,RST,PSH,ACK,URG NONE -j DROP
iptables -A tcpfilter -p tcp -m tcp --tcp-flags SYN,RST SYN,RST -j DROP
iptables -A tcpfilter -p tcp -m tcp --tcp-flags FIN,SYN FIN,SYN -j DROP

# PREROUTING
iptables -N PREROUTING
iptables -A PREROUTING -d 10.0.0stio.1/32 -i eth0 -p tcp -m tcp --dport 25 -j DNAT --to-destination 192.168.1.254
iptables -A PREROUTING -d 10.0.0.1/32 -i eth0 -p tcp -m tcp --dport 110 -j DNAT --to-destination 192.168.10.254
iptables -A PREROUTING -d 10.0.0.1/32 -i eth0 -p tcp -m tcp --dport 444 -j DNAT --to-destination 192.168.10.254:443
iptables -A PREROUTING -d 10.0.0.1/32 -i eth0 -p tcp -m tcp --dport 80 -j DNAT --to-destination 192.168.10.253
iptables -A POSTROUTING -s 192.168.1.0/24 -o eth0 -j SNAT --to-source 10.0.0.1
iptables -A POSTROUTING -o eth0 -j MASQUERADE

# Filter IRC traffic
iptables -A INPUT -p tcp -m tcp --dport 194 -j DROP     # IRC
iptables -A INPUT -p udp -m udp --dport 194 -j DROP
iptables -A INPUT -p tcp -m tcp --dport 529 -j DROP     # IRC-SERV
iptables -A INPUT -p udp -m udp --dport 529 -j DROP
iptables -A INPUT -p tcp -m tcp --dport 994 -j DROP     # IRC over TLS/SSL
iptables -A INPUT -p udp -m udp --dport 994 -j DROP
iptables -A INPUT -p tcp -m tcp --dport 6667 -j DROP    # IRC; many trojans/backdoors use this port
iptables -A INPUT -p udp -m udp --dport 6667 -j DROP

# Filter traffic to UDP port 0 (reserved; TCP has no port 0...) and port 1 (TCP Port Service Multiplexer - TCPMUX)
iptables -A INPUT -i eth0 -p tcp -m tcp --tcp-flags FIN,SYN,RST,ACK SYN -j DROP
iptables -A INPUT -i eth0 -p tcp -m tcp --dport 0 -j DROP
iptables -A INPUT -i eth0 -p udp -m udp --dport 0 -j DROP
iptables -A INPUT -i eth0 -p tcp -m tcp --dport 1 -j DROP
iptables -A INPUT -i eth0 -p udp -m udp --dport 1 -j DROP

iptables -A INPUT -j DROP
iptables -A OUTPUT -j DROP
iptables -A FORWARD -j DROP
