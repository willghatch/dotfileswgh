#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    echo "Usage: $0"
    echo "Update DNS A and AAAA records for the current host's IP addresses via nsupdate."
    echo "Requires environment variables: DNSSERVER, DNSZONE, DNSHOST, KEYFILE."
    exit 0
fi

if [[ -z "$DNSSERVER" || -z "$DNSZONE" || -z "$DNSHOST" || -z "$KEYFILE" ]]; then
    echo "variables DNSSERVER DNSZONE DNSHOST KEYFILE are required"
    exit 1
fi

ip6=$(ifconfig | grep inet6 | grep global | awk '{print  $2}' | head -n 1)
ip4=$(ifconfig | grep inet | grep -v inet6 | awk '{print $2}' | grep -E -v '127.0.0.1|192\.168\.[[:alnum:]]+\.[[:alnum:]]+|10.[[:alnum:]]+\.[[:alnum:]]+\.[[:alnum:]]+' | head -n 1)

zfile=/tmp/zoneupdate

# create new empty zone update file
echo "" > $zfile

echo "server $DNSSERVER" >>$zfile
echo "zone $DNSZONE" >>$zfile
echo "update delete ${DNSHOST}.${DNSZONE}. A" >>$zfile
echo "update delete ${DNSHOST}.${DNSZONE}. AAAA" >>$zfile
if [[ -n "$ip4" ]]; then
    echo "update add ${DNSHOST}.${DNSZONE}. 3600 A $ip4" >>$zfile
fi
if [[ -n "$ip6" ]]; then
    echo "update add ${DNSHOST}.${DNSZONE}. 3600 AAAA $ip6" >>$zfile
fi
echo "show" >>$zfile
echo "send" >>$zfile

nsupdate -k "$KEYFILE" -v "$zfile"
