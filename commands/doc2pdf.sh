#!/bin/sh


if [ -n "$(ps -e | grep soffice)" ]
then
  echo 2>&1 "There is already and instance of libreoffice running,"
  echo 2>&1 "and for some reason this has to fail if that's the case."
  echo 2>&1 "NO CONVERSION MADE!"
  exit 37
fi

for file in $@
do
  if [ ! -r $file ]
  then
    echo 2>&1 "$file: file is unreadable or does not exist.  No conversion made."
    exit 38
  fi
done

libreoffice --headless --convert-to pdf $@

