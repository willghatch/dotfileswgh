#!/usr/bin/env bash

# install the packages I generally use on Arch Linux

pm (){
    pacman -S --needed --noconfirm "$@"
}

pm base base-devel openssh net-tools wpa_supplicant networkmanager wireless_tools
pm git
pm zsh vim emacs tmux elinks

pm xorg konsole xterm xorg-xinit arandr bashrun ttf-dejavu xclip
pm python python2 python-pip python2-pip ghc cabal-install racket nodejs npm
pm firefox
pm sshfs zip unzip evince tig

pm \
pkgfile \
the_silver_searcher \
feh \
awesome \
vicious \
lxterminal \
xfce4-terminal \
chromium \
xterm \
xscreensaver \
pm-utils \
unclutter \
vorbis-tools \
vlc \
wget \
mpd \
mpc \
ncmpc \
espeak \
qemu \
network-manager-applet \
blueman \
mlocate \
ctags \
clisp \
alsa-utils \
abcde \
lxappearance \
fortune-mod \
cowsay \
xcompmgr \
mutt \
pidgin \
finch \
tk \
python2-dbus \
markdown \
bind-tools \
libnotify \
dunst \
texlive-bin

# for teensy programming (IE ergodox)
pm avr-binutils avr-gcc avr-libc

