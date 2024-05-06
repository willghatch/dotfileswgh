#!/usr/bin/env bash


if [[ "$@" =~ "--help" ]]; then
    echo "$0 <optional: program and its args> ..."
    exit 1
fi

newWorkspaceNumber="$(( $(hyprctl workspaces | grep "workspace ID" | cut -d " " -f 3 | sort -rn | head -1) + 1 ))"

hyprctl dispatch workspace "${newWorkspaceNumber}"

# command from the internet...

if [[ -n "$1" ]]; then
    #vlaunch terminal &
    "$@" &
fi

