#!/usr/bin/env bash

# install the packages I generally use on Arch Linux

pm (){
    pacman -S --needed --noconfirm "$@"
}

# get basic packages
pm base base-devel
# networking
pm openssh net-tools wpa_supplicant networkmanager wireless_tools

# get my basic tools
pm git
pm zsh
pm vim
pm tmux
pm elinks
pm emacs

# next tier of basic things
pm xorg konsole xterm xorg-xinit arandr bashrun ttf-dejavu xclip
pm python python2 python-pip python2-pip ghc cabal-install racket nodejs npm
pm firefox
pm sshfs zip unzip tig
pm zathura zathura-pdf-poppler zathura-ps zathura-djvu

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
gnome-keyring \
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
# for mkfs.vfat
pm dosfstools

# for xdg-open type stuff
pm perl-file-mimeinfo
# because it's decent and small, so it can be the default file manager
pm pcmanfm
