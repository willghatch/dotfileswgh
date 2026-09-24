#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    echo "Usage: $0"
    echo "Launch startup programs for a Floatile session: the bar, then everything in wayland-startup-programs.sh."
    echo "Run by startup-commands in the Floatile configuration."
    exit 0
fi

# TODO - replace Waybar with something Floatile-specific.
waybar-configured &

exec wayland-startup-programs.sh
