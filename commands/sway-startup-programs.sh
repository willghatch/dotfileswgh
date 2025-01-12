#!/usr/bin/env bash


# This is executed at the end of sway configuration.
# Sway should be launched in some way such that it already has env-more sourced before launch.
#source $DOTFILESWGH/env-more.sh

swayidle-configured &

# TODO - I'm frustrated with both of these bars.  Waybar keeps crashing, but ironbar has issues when a monitor disconnects/reconnects (eg. due to idle).
waybar-configured &
#ironbar-configured &

wallpaper-sway-random-rotate &

wl-gammarelay-rs &

# We still need xsettingsd for xwayland programs and such.
xsettingsd-wrapper --start &

# TODO - touchpad tap-to-click and horizontal scroll?  At least tap-to-click I have in my nixos config now.

# TODO - keynav replacement?
# TODO - unclutter replacement (hide mouse cursor)
# TODO - something to detect and set monitor config, or apply the default for this machine, or something
# TODO - wallpaper set random
# TODO - set initial mouse cursor location?


## --- This comment was from probably 2017, and is probably out of date.  But I'll leave it here for easy reference if I notice anything fishy.
## The keyboard layout setting via environment variable leaves me with some
## weirdness for some programs that this solves.
## I really don't understand what's going on.
## It seems like some programs are still looking at some sort of X config
## that wayland doesn't know about.  Ideally the commands used to set the keyboard
## state would just work for both.
#hkk

if [ -f $DOTFILESWGH_PRI/sway-startup-rc ]; then
    source $DOTFILESWGH_PRI/sway-startup-rc
fi
if [ -f $HOME/rootgit-dotfiles/sway-startup-rc ]; then
    source $HOME/rootgit-dotfiles/sway-startup-rc
fi
if [ -f $DOTFILESWGH_DOTLOCAL/sway-startup-rc ]; then
    source $DOTFILESWGH_DOTLOCAL/sway-startup-rc
fi


source $DOTFILESWGH/gui-session-common

