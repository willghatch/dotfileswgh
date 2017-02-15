#!/usr/bin/env bash

pm (){
    pacman -S --needed --noconfirm "$@"
}


# pacman packages
pm gcc-multilib git gnupg flex bison gperf sdl wxgtk squashfs-tools curl ncurses zlib schedtool perl-switch zip unzip libxslt python2-virtualenv bc rsync lib32-zlib lib32-ncurses lib32-readline

# lineage-os additionally needs these
pm xml2 lzop pngcrush schedtool squashfs-tools lzop imagemagick


# cyanogenmod/lineage 14 and android 7 switched to java 8, and officially requires openjdk
pm jdk8-openjdk

# Extra stuff
pm repo android-tools android-udev

echo "installed official repo packages needed, remember to install AUR packages"

