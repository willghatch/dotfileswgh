#!/bin/sh

# This should all run after I have a session bus for d-bus.
# Some things here connect to d-bus as they launch, so I can't put dbus-launch
# wrapping the exec to my window manager.  But gnome-terminal doesn't see it
# if I just launch it in this script.  So I'll use dbus-launch to launch this.

#pulseaudio &

source $DOTFILESWGH/xsession-common


if [ -f $DOTFILESWGH/pri/xinitrc ]; then
    source $DOTFILESWGH/pri/xinitrc
fi
if [ -f $DOTFILESWGH/dotlocal/xinitrc ]; then
    source $DOTFILESWGH/dotlocal/xinitrc
fi

exec awesome

