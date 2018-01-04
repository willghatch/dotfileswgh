#!/usr/bin/env bash

msg=i3-msg
if [[ -n "$SWAYSOCK" ]]; then
    msg=swaymsg
fi


# command from the internet...
$msg workspace $(($($msg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1) + 1))
vlaunch terminal &

