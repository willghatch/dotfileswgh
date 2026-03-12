#!/bin/sh

case "$*" in *--help*)
    echo "Usage: $0"
    echo "Rename files with spaces to underscores, recursing into directories."
    exit 0
    ;; esac

ls | while read -r FILE
do
    mv -v "$FILE" `echo $FILE | tr ' ' '_' `
done

ls | while read -r FILE
do
    if [ -d "$FILE" ]
    then
        cd $FILE
        sh $0
        cd ..
    fi
done

