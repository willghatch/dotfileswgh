#!/usr/bin/env bash

# find which library a symbol is in

if [[ -z "$1" || "--help" = "$1" ]]; then
  echo "usage: $0 <symbol-to-find>"
fi

scanelf -l -s "$1" | grep "$1"

