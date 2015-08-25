#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  echo "usage: $0 <ergodox firmware .hex file>"
  exit 1
fi

teensy-loader-cli -mmcu=atmega32u4 -w -v "$1"

