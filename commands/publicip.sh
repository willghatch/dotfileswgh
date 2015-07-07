#!/bin/sh

# get my public IP address


if which dig >/dev/null 2>&1; then
    # using dns, if possible.  Note, dig is in the dnsutils package in Arch.
    dig +short myip.opendns.com @resolver1.opendns.com
else

    # using web services

    # there are various alternatives
    # instead of curl you could use 
    getpage="curl"
    #getpage="wget -qO-"
    
    #$getpage icanhazip.com
    $getpage ipv4.icanhazip.com
    #$getpage ipv6.icanhazip.com
    #$getpage ident.me
    #$getpage v4.ident.me
    #$getpage v6.ident.me
    #$getpage ipecho.net/plain
    #$getpage ifconfig.me
    #$getpage -s checkip.dyndns.org | sed -e 's/.*Current IP Address: //' -e 's/<.*$//'

fi
