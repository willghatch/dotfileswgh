#!/bin/sh
# gammarelay-watch.sh FORMAT -- print wl-gammarelay-rs values in FORMAT
# (as `wl-gammarelay-rs watch' takes it) each time they change.
#
# The bar can start before the wl-gammarelay-rs daemon registers on D-Bus,
# and the daemon can restart, which ends `watch'; so keep watching, and
# print an empty line while there is nothing to watch, which hides the item.
while :; do
    wl-gammarelay-rs watch "$1" 2>/dev/null
    echo ""
    sleep 3
done
