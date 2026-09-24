#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    echo "Usage: $0"
    echo "Launch startup programs for a Hyprland session: the bar, then everything in wayland-startup-programs.sh."
    echo "Run by exec-once in the Hyprland configuration."
    exit 0
fi

# TODO - I'm frustrated with both of these bars.  Waybar keeps crashing, but ironbar has issues when a monitor disconnects/reconnects (eg. due to idle).
waybar-configured &
#ironbar-configured &

exec wayland-startup-programs.sh
