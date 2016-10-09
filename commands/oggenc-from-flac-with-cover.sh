#!/usr/bin/env bash

flacfile="$1"
oggfile="${1/flac/ogg}"

oggenc "$flacfile" && ogg-add-jpeg-cover.sh "$oggfile" <(metaflac --export-picture-to=- "$flacfile")

