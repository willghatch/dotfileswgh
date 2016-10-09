#!/usr/bin/env bash

if [[ "$#" -lt "2" ]]; then
    echo "usage: $0 <file.ogg> <file.jpg>"
    echo "Add "
    exit 1
fi

OGG="$1"
ogg_basename="$(basename "$OGG")"
PIC_ORIG="$2"
PIC="/tmp/${ogg_basename}.jpg"
# copy, so I can have a pipe for the pic file
cp "$PIC_ORIG" "$PIC"


DESC=`basename "$PIC"`
APIC=`base64 --wrap=0 "$PIC"`
#if [ "`which exiv2`" != "" ]; then
#MIME=`exiv2 "$PIC" | grep 'MIME type ' | sed 's/: /|/' | cut -f 2 -d '|' | tail -n 1`
#fi
#if [ "$MIME" = "" ]; then
#MIME="image/jpeg"
#fi
MIME="image/jpeg"

tags1="/tmp/${ogg_basename}.tags1"
tags2="/tmp/${ogg_basename}.tags2"

vorbiscomment -l "$OGG" | grep -v '^COVERART=' | grep -v '^COVERARTDESCRIPTION=' | grep -v '^COVERARTMIME=' | grep -v 'METADATA_BLOCK_PICTURE=' > "$tags1"

#echo METADATA_BLOCK_PICTURE="$APIC" > "$tags2"
# easytag understands the coverart tag
echo COVERART="$APIC" > "$tags2"

vorbiscomment -w -R -c "$tags2" "$OGG"
#vorbiscomment -a -R -t COVERARTDESCRIPTION="$DESC" "$OGG"
vorbiscomment -a -R -t COVERARTMIME="$MIME" "$OGG"
vorbiscomment -a -R -c "$tags1" "$OGG"

rm -f "$tags2"
rm -f "$tags1"
rm -f "$PIC"
