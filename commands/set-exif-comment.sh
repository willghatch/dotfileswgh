#!/usr/bin/env bash

if [[ $# -lt 2 ]]; then
  echo "usage: $0 <file.jpg> <new comment to set>"
  exit 1
fi

exiv2 -M"set Exif.Photo.UserComment \"$2\"" $1

