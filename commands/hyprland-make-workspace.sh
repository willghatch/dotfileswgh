#!/usr/bin/env bash


if [[ "$@" =~ "--help" ]]; then
    echo "$0 <optional: program and its args> ..."
    exit 1
fi

if [[ "$1" = "--move-current-window" ]]; then
    shift
    windowToMove="$(hyprctl -j activewindow | jq .pid)"
fi

function firstMissingNumber() {
    last_number=0
    missing_number_found=false

    while read -r number; do
        if [[ $number -gt $(($last_number + 1)) && "$missing_number_found" = false ]]; then
            echo $(($last_number + 1))
            missing_number_found=true
            break
        fi
        last_number=$number
    done
    if [[ "$missing_number_found" = false ]]; then
        echo $(($last_number + 1))
    fi
}

newWorkspaceNumber="$(( $(hyprctl workspaces | grep "workspace ID" | cut -d " " -f 3 | sort -rn | head -1) + 1 ))" # This one is annoying because when a workspace is missing, it keeps incrementing the numbers, then there are holes.
#newWorkspaceNumber="$(hyprctl workspaces | grep "workspace ID" | cut -d " " -f 3 | sort -n | firstMissingNumber)" # This one is annoying because the new workspace goes in the middle of the existing workspaces, where the missing number was, whereas I want the new workspace to appear on the right.

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

