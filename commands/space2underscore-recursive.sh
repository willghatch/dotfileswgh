#!/bin/sh

# Rename files with spaces to underscores.  Recurse into directories.

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

