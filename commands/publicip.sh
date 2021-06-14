#!/usr/bin/env bash

# get my public IP address

if [[ "$1" = -6 ]]; then
    ip6=true
fi
if which dig >/dev/null 2>&1; then
    hasdig=true
fi

if [[ "$hasdig" = true && "$ip6" != true ]]; then
    # using dns, if possible.  Note, dig is in the dnsutils package in Arch.
    # Be sure to use ipv4 to make the request, or it will want to respond with ipv6 info.
    dig -4 +short myip.opendns.com @resolver1.opendns.com
else

    # using web services

    # there are various alternatives
    # instead of curl you could use 
    getpage="curl"
    #getpage="wget -qO-"

    page4=ipv4.icanhazip.com
    page6=ipv6.icanhazip.com
    
    #page4=icanhazip.com
    #page4=ipv4.icanhazip.com
    #page6=ipv6.icanhazip.com
    #page4=ident.me
    #page4=v4.ident.me
    #page6=v6.ident.me
    #page4=ipecho.net/plain
    #page4=ifconfig.me

    if [[ "$ip6" = true ]]; then
        page=$page6
    else
        page=$page4
    fi

    "$getpage" "$page"

fi
