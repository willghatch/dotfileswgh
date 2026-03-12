#/bin/sh

# The dd command gives a status update when it receives a USR1 signal.
# This is an easy wrapper to get regular updates.

if [[ "$@" =~ "--help" || "$#" = 0 ]]; then
    cat $(readlink -f "$0")
    exit 0
fi

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

