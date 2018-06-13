#!/usr/bin/env bash

# install the packages I generally use on Arch Linux

# TODO - split this into sections
#      - core things that I want on every system (even phone, cloud host, etc)
#      - gui things I want on a beefy laptop (everything just works, etc)
#      - maybe smaller divisions.

pm (){
    pacman -S --needed --noconfirm "$@"
}

# get basic packages
pm base base-devel
# networking
pm openssh net-tools wpa_supplicant networkmanager wireless_tools

# get my basic tools
pm git
pm stow
pm zsh
pm vim
pm tmux
pm elinks
pm emacs

# next tier of basic things
pm xorg konsole xterm xorg-xinit arandr ttf-dejavu xclip
# I guess I need vdpau to have hardware acceleration for video
pm mesa-vdpau
pm python python2 python-pip python2-pip racket nodejs npm
#pm ghc cabal-install
pm firefox
pm sshfs zip unzip tig
pm zathura zathura-pdf-poppler zathura-ps zathura-djvu
pm texlive-most

pm sway i3status

pm \
pkgfile \
htop \
the_silver_searcher \
feh \
awesome \
lua-socket \
vicious \
lxterminal \
xfce4-terminal \
gnome-terminal \
chromium \
android-udev \
android-tools \
repo \
ccache \
ninja \
xterm \
xscreensaver \
unclutter \
vorbis-tools \
ddrescue \
gnome-mplayer \
vlc \
libdvdcss \
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
alsa-utils \
pulseaudio \
pulseaudio-alsa \
pulseaudio-zeroconf \
pulseaudio-bluetooth \
pavucontrol \
pamixer \
lxappearance \
fortune-mod \
cowsay \
xcompmgr \
mutt \
notmuch \
pidgin \
finch \
gajim \
tk \
python2-dbus \
markdown \
bind-tools \
libnotify \
dunst \
rlwrap \
pygmentize \
texlive-bin

pm pm-utils

# cd ripping -- glyr is needed for fetching album art
# !! also need aur packages
pm abcde glyr

pm fail2ban

# for teensy programming (IE ergodox)
pm avr-binutils avr-gcc avr-libc
# for mkfs.vfat
pm dosfstools
# for mounting iso images without root
pm fuseiso
# for keeping encrypted dirs and mounting the decrypted version
pm encfs

# check hard drive health
pm smartmontools

# for flashing infinity ergodox
pm dfu-util

# for xdg-open type stuff
pm perl-file-mimeinfo
# because it's decent and small, so it can be the default file manager
pm pcmanfm

# cron and at
pm cronie at

# zeroconf
pm avahi nss-mdns

# for utilities like lpr
pm cups

# to inspect keychains and such
pm seahorse

# I want common libraries to just be installed.  I don't need to save disk space.
#pm qt5 qt4 gtk3 gtk2

pm tigervnc
pm nmap
pm borgbackup

pm syncthing
pm syncthing-gtk
pm offlineimap
pm msmtp

# hopefully using this increases battery life...
pm cpupower
pm tlp

# send key events to xserver
pm xdotool

# to see image metadata
pm exiv2

# image viewer that's easy to edit exif comment tag with, etc
pm gthumb

# pacaur dependency
pm expac

# time update
pm ntp

pm qrencode
