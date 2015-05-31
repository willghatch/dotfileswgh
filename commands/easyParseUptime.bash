#!/usr/bin/env bash

# Shows uptime in a format easy to parse
# days hours minutes seconds
# separated by spaces

# Author: William Hatch

upTime=$(cat /proc/uptime | awk -F. '{print $1}')

seconds=$(($upTime % 60))
minutes=$(($upTime / 60 % 60))
hours=$(($upTime / 3600 % 24))
days=$(($upTime / 86400))

echo "$days $hours $minutes $seconds"
