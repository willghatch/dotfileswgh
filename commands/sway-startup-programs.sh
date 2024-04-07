#!/usr/bin/env bash


# This is executed at the end of sway configuration.
# Sway should be launched in some way such that it already has env-more sourced before launch.
#source $DOTFILESWGH/env-more.sh

# TODO - idle management - how to toggle this off/on?  Eg. if I'm watching a movie I don't want the screen to blank.  Probably I need to wrap this in a script and turn it on and off, or check a state file, or something.
# swayidle *should* be toggleable on my waybar config
swayidle -w \
	timeout 300 'swaylock-configured' \
	timeout 600 'swaymsg "output * power off"' \
		resume 'swaymsg "output * power on"' \
	before-sleep 'swaylock-configured' &

waybar-configured &


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

if [ -f $DOTFILESWGH/pri/sway-startup-rc ]; then
    source $DOTFILESWGH/pri/sway-startup-rc
fi
if [ -f $HOME/rootgit-dotfiles/sway-startup-rc ]; then
    source $HOME/rootgit-dotfiles/sway-startup-rc
fi
if [ -f $DOTFILESWGH/dotlocal/sway-startup-rc ]; then
    source $DOTFILESWGH/dotlocal/sway-startup-rc
fi


source $DOTFILESWGH/gui-session-common

