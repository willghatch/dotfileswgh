#!/usr/bin/env bash

# Remove the default flag from the first audio and subtitle tracks.
# Hopefully the first one is the only one so marked...

if [[ "$@" =~ "--help" || "$#" = 0 ]]; then
    cat $(readlink -f "$0")
    exit 0
fi

for f in "$@"; do
  mkvpropedit "$f" \
    --edit track:a1 --set flag-default=0 \
    --edit track:s1 --set flag-default=0
done

