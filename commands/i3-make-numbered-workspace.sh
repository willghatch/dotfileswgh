#!/usr/bin/env bash

msg=i3-msg
if [[ -n "$SWAYSOCK" ]]; then
    msg=swaymsg
fi

if [[ "$@" =~ "--help" ]]; then
    echo "$0 <optional args for program> ..."
    exit 1
fi


# command from the internet...
$msg workspace $(($($msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))

if [[ -n "$1" ]]; then
    #vlaunch terminal &
    "$@" &
fi

