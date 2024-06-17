#!/usr/bin/env bash


if [[ "$@" =~ "--help" ]]; then
    echo "$0 <optional: program and its args> ..."
    exit 1
fi

if [[ "$1" = "--move-current-window" ]]; then
    shift
    windowToMove="$(hyprctl -j activewindow | jq .pid)"
fi

newWorkspaceNumber="$(( $(hyprctl workspaces | grep "workspace ID" | cut -d " " -f 3 | sort -rn | head -1) + 1 ))"

hyprctl dispatch workspace "${newWorkspaceNumber}"

# command from the internet...

if [[ -n "$windowToMove" ]]; then
    echo "in windowToMove case" > /tmp/log
    hyprctl dispatch movetoworkspace "${newWorkspaceNumber},pid:${windowToMove}"
    echo hyprctl dispatch movetoworkspace "${newWorkspaceNumber},pid:${windowToMove}" >>/tmp/log
fi

if [[ -n "$1" ]]; then
    #vlaunch terminal &
    "$@" &
fi

