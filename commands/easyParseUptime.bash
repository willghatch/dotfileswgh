#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    cat $(readlink -f "$0")
    exit 0
fi

upTime=$(cat /proc/uptime | awk -F. '{print $1}')

seconds=$(($upTime % 60))
minutes=$(($upTime / 60 % 60))
hours=$(($upTime / 3600 % 24))
days=$(($upTime / 86400))

echo "$days $hours $minutes $seconds"
