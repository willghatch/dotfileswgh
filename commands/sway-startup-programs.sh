#!/usr/bin/env bash

# xinitrc replacement for sway, essentially

pulseaudio &

# merge xresources first, so other programs in this (xscreensaver) can get settings
#xrdb $DOTFILESWGH/Xresources
lightdark-status soft-update

# The keyboard layout setting via environment variable leaves me with some
# weirdness for some programs that this solves.
# I really don't understand what's going on.
# It seems like some programs are still looking at some sort of X config
# that wayland doesn't know about.  Ideally the commands used to set the keyboard
# state would just work for both.
hkk
# I don't know if there is a way to do this and make it actually work on wayland.
#xkbset sticky -twokey latchlock


# TODO - something like this
# Unclutter hides the mouse pointer after the given number of seconds
#unclutter -idle 5 &


# TODO - how to set monitors?  By default it seems to do a good job, but if I want
# something non-default or dynamic what do I do?

# TODO - dynamic wallpaper setting in sway

# TODO - how to do this?
#### touchpad config
# turn off tap-to-click
#synclient maxtaptime=0
# allow horizontal two-finger scroll
#synclient HorizTwoFingerScroll=1

# Notifications
dunst &
$DOTFILESWGH/external/misc/libnotifylog/libnotifylogger.py $HOME/.cache/notifications.log &

# music!
mpd &

# tray apps
# TODO - tray support doesn't exist yet
#nm-applet &
#blueman-applet &

# The unicoder doesn't work on wayland right now.  But eventually I'll fix that.
unicoder_socket="/tmp/the-unicoder_${USER}_${DISPLAY/:/}"
rm -f "$unicoder_socket"
the-unicoder --server --path "$unicoder_socket" &

