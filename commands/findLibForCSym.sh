#!/usr/bin/env bash

# find which library a symbol is in

if [[ "$@" =~ "--help" || "$#" = 0 ]]; then
    cat $(readlink -f "$0")
    exit 0
fi

scanelf -l -s "$1" | grep "$1"

