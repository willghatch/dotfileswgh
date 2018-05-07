#!/usr/bin/env bash

put(){
    echo -e "\e[1;35m∇∇∇ installing $@\e[0m"
    nix-env -i "$@"
}


pkgs=()
while read pkg; do
    # if the line is non-empty and doesn't start with # then it's a package name.
    if [[ -n "$pkg" && "${pkg:0:1}" != "#" ]]; then
        #put "$pkg"
        pkgs=(${pkgs[@]} $pkg)
    fi
done <<END_OF_PACKAGES
# This is basically just a copy of my guix package script

# networking
net-tools
wireless-tools
wpa_supplicant
network-manager
network-manager-applet
gnome-keyring
# I think bind probably has what bind-utils has elsewhere?
bind
socat
curl
wget
openssh
fail2ban
blueman
syncthing

# compression/archives
gzip
xz
bzip2
gnutar
zip
unzip
# TODO
#unrar
#7z

# git and friends
git
tig
mercurial
subversion
# for gitk, and really nothing else
tk

# editors
vim
emacs

# languages, build tools
bash
zsh
racket
guile
gcc
lua
gnumake
automake
cmake
python
python3.6-pip
python2.7-pip
ghc
#cabal-install
nodejs
ctags

# X11
xorg-server
xrdb
xset
xsetroot
xinit
setxkbmap
xkbcomp
xmodmap
arandr
xclip
fluxbox
# hide mouse cursor
unclutter
lxappearance
awesome
vicious
xscreensaver
#TODO
#cinnamon
lxqt-session

# fonts
pango
freetype
fontconfig
dejavu-fonts
font-misc-misc
liberation-fonts
terminus-font-ttf
noto-fonts
inconsolata
unifont
fantasque-sans-mono

# A decent default file browser
pcmanfm

# file systems
sshfs-fuse
# fuse-exfat is just exfat in nix I guess
exfat
dosfstools
encfs

# web browsers
elinks
links2
w3m
lynx
chromium
firefox

# terminals
tmux
xterm
xfce4-terminal
gnome-terminal
konsole

# images
feh

# pdf
zathura-with-plugins

# DAV, calendar, contacts
khard
khal
vdirsyncer

# email, messaging
offlineimap
mutt
gajim

# audio
vorbis-tools
flac
alsa-utils
pavucontrol
mpd
mpc
ncmpc
abcde
vlc

# writing
texlive-combined-full
#TODO - I'm sure there's a markdown package, but I can't find its name
#markdown

# notifications
libnotify
dunst
python2.7-dbus-python

# power management utils
pm-utils

# misc
qemu
man-pages
silver-searcher
espeak
mlocate

END_OF_PACKAGES

put "${pkgs[@]}"
