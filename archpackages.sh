#!/usr/bin/env bash

pm (){
    pacman -S --needed --noconfirm "$@"
}

pm base base-devel openssh net-tools wpa_supplicant networkmanager wireless_tools
pm git
pm zsh vim emacs tmux elinks

pm xorg konsole xterm xorg-xinit arandr bashrun ttf-dejavu
pm python python2 python-pip python2-pip ghc cabal-install racket nodejs npm
pm firefox
pm sshfs zip unzip evince tig

pm \
feh \
awesome \
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
texlive-bin

