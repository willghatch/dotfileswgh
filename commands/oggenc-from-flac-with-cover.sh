#!/usr/bin/env bash

if [[ "$@" =~ "--help" || "$#" -lt 2 ]]; then
  echo "usage: $0 <flacfile> <outfile>"
  echo "Encode a FLAC file to OGG and copy the embedded cover art."
  exit 1
fi

flacfile="$1"
oggfile="${1/flac/ogg}"
outfile="$2"

oggenc "$flacfile" -o "$outfile" && ogg-add-jpeg-cover.sh "$outfile" <(metaflac --export-picture-to=- "$flacfile")

