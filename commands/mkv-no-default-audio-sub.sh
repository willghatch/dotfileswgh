#!/usr/bin/env bash

# Remove the default flag from the first audio and subtitle tracks.
# Hopefully the first one is the only one so marked...

if [[ "$#" -lt 1 || "$1" = "--help" ]]; then
  echo "Remove the default flag from the first audio and subtitle tracks of an mkv"
  echo "file, which some mkv-making programs mark by default for some reason."
  echo "Usage: $0 <mkv-file-to-un-default> [more files ...]"
  exit 1
fi

for f in "$@"; do
  mkvpropedit "$f" \
    --edit track:a1 --set flag-default=0 \
    --edit track:s1 --set flag-default=0
done

