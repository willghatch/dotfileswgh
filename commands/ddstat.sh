#/bin/sh

interval=$1

if [ -d $interval ]
then
  interval="5"
fi

while [ true ]
do
  killall dd -USR1
  sleep $interval
done

