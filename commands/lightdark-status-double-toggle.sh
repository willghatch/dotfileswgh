#!/usr/bin/env bash

if [[ "$@" =~ "--help" ]]; then
    cat $(readlink -f "$0")
    exit 0
fi

lightdark-status toggle
sleep 1
lightdark-status toggle
