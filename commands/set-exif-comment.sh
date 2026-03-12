#!/usr/bin/env bash

if [[ "$@" =~ "--help" || "$#" -ne 2 ]]; then
  echo "Usage: $0 <file.jpg> <new comment to set>"
  echo "Set the EXIF UserComment field on a JPEG image using exiv2."
  exit 0
fi

exiv2 -M"set Exif.Photo.UserComment \"$2\"" $1

