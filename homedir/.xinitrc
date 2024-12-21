#!/bin/sh
export WGHHOME=${WGHHOME:-$HOME}
export DOTFILESWGH=${DOTFILESWGH:-$WGHHOME/dotfileswgh}
export DOTFILESWGH_PRI=${DOTFILESWGH_PRI:-$WGHHOME/dotfileswgh-pri}
export DOTFILESWGH_DOTLOCAL=${DOTFILESWGH_DOTLOCAL:-$WGHHOME/dotfileswgh-dotlocal}
# run the REAL xinitrc with dbus-launch
export LOGIN_TTY=`tty`
exec dbus-launch --exit-with-x11 $DOTFILESWGH/xinitrc-post-dbus.sh

