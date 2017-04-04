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
htop \
the_silver_searcher \
feh \
awesome \
lua-socket \
vicious \
lxterminal \
xfce4-terminal \
chromium \
android-udev \
android-tools \
repo \
ccache \
ninja \
xterm \
xscreensaver \
pm-utils \
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
clisp \
alsa-utils \
pulseaudio \
pulseaudio-alsa \
pulseaudio-zeroconf \
pulseaudio-bluetooth \
pavucontrol \
lxappearance \
fortune-mod \
cowsay \
xcompmgr \
mutt \
notmuch \
pidgin \
finch \
tk \
python2-dbus \
markdown \
bind-tools \
libnotify \
dunst \
texlive-bin

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

# zeroconf
pm avahi nss-mdns

# I want common libraries to just be installed.  I don't need to save disk space.
pm qt5 qt4 gtk3 gtk2

