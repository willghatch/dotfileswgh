#!/bin/sh
export WGHHOME=${WGHHOME:-$HOME}
export DOTFILESWGH=${DOTFILESWGH:-$WGHHOME/dotfileswgh}
source $DOTFILESWGH/env-more.sh
# run the REAL xinitrc with dbus-launch
export LOGIN_TTY=`tty`
exec dbus-launch --exit-with-x11 $DOTFILESWGH/xinitrc-post-dbus.sh

