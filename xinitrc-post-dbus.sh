#!/bin/sh

# This should all run after I have a session bus for d-bus.
# Some things here connect to d-bus as they launch, so I can't put dbus-launch
# wrapping the exec to my window manager.  But gnome-terminal doesn't see it
# if I just launch it in this script.  So I'll use dbus-launch to launch this.

# merge xresources first, so other programs in this (xscreensaver) can get settings
xrdb $DOTFILESWGH/Xresources

# get keyboard layout
hkk
# set sticky keys, don't cancel sticky keys if two keys are pressed at once, allow latch-to-lock
xkbset sticky -twokey latchlock

# Control mouse with keyboard
keynav &

# Unclutter hides the mouse pointer after the given number of seconds
unclutter -idle 5 &

# set the screen layout -- IE detect how many monitors I have connected and adjust
screenlayout-default.sh

# Backround
{
    while true; do
        wallpaper-set-random
        # 600 seconds = 10 minutes
        sleep 600
    done
}&

#cinnamon-screensaver &
xscreensaver -no-splash &

# I hate it tappnig the touchpad counts as a mouse click.
stopTouchpadClick.sh &

# Be sure I have a decent terminfo available
tic $DOTFILESWGH/xterm-256color-italic.terminfo

# Notifications
dunst &

mpd &

# Compositing for eg. transparent terminals
xcompmgr &

# application tray
# good trayer command
#trayer --edge top --align right --width 10 --height 12 --transparent true --alpha 255 &
nm-applet &
blueman-applet &

unicoder_socket="/tmp/the-unicoder_${USER}_${DISPLAY}"

rm -f "$unicoder_socket"
the-unicoder --server --path "$unicoder_socket" &

$DOTFILESWGH/external/misc/libnotifylog/libnotifylogger.py $HOME/.cache/notifications.log &

# Watch keyboard state for awesome
$DOTFILESWGH/config/awesome/kbd-state-mon.sh &

if [ -f $DOTFILESWGH/pri/xinitrc ]; then
    source $DOTFILESWGH/pri/xinitrc
fi
if [ -f $DOTFILESWGH/dotlocal/xinitrc ]; then
    source $DOTFILESWGH/dotlocal/xinitrc
fi

exec awesome

