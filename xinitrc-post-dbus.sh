#!/bin/sh

# This should all run after I have a session bus for d-bus.
# Some things here connect to d-bus as they launch, so I can't put dbus-launch
# wrapping the exec to my window manager.  But gnome-terminal doesn't see it
# if I just launch it in this script.  So I'll use dbus-launch to launch this.

pulseaudio &

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
# this script should live in one of my local script directories on my path
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

#### touchpad config
# turn off tap-to-click
synclient maxtaptime=0
# allow horizontal two-finger scroll
synclient HorizTwoFingerScroll=1
# make two-finger scroll slower.  Default is 108, higher is slower (IE cover more trackpad per scroll signal)
# 300 is slower than I want in gnome-terminal and firefox, but still too fast for racket text fields...
synclient VertScrollDelta=300
synclient HorizScrollDelta=300


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

