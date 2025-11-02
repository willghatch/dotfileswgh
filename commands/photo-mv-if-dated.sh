#!/usr/bin/env bash

imgfile="$1"

date=$(exiftool $imgfile -EXIF:CreateDate | awk '{print $4,$5}' | sed "s/ /_/" | sed s/:/-/g)

if [[ "$date" = 0000-00-00_00-00-00 ]]; then
  echo "No date"
elif [[ "$date" = "" ]]; then
  echo "No date"
else
  echo mv -i "$imgfile" "${date}.jpg"
  mv -i "$imgfile" "${date}.jpg"
fi

