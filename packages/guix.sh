#!/usr/bin/env bash

put(){
    echo -e "\e[1;35m∇∇∇ installing $@\e[0m"
    guix package -i "$@"
}


while read pkg; do
    # if the line is non-empty and doesn't start with # then it's a package name.
    if [[ -n "$pkg" && "${pkg:0:1}" != "#" ]]; then
        put "$pkg"
    fi
done <<END_OF_PACKAGES
# TODO
# For now, and probably for quite a while, I will be running Guix inside another OS,
# rather than running GuixSD.  So a number of packages (kernel, system daemons) will
# actually be those of the host OS rather than of Guix, unless I go out of my way to
# use the Guix package instead.
#
# But at some point, I need to figure out how to run Guix all the way down.

# networking
net-base
net-tools
wireless-tools
wpa-supplicant
network-manager
network-manager-applet
gnome-keyring
bind-utils
socat
curl
wget
# TODO
#openssh
#fail2ban
#blueman
#syncthing

# compression/archives
gzip
xz
bzip2
tar
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
make
automake
cmake
python
python-pip
python2-pip
ghc
cabal-install
node
# TODO
#ctags

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
# TODO
#awesome
#vicious
#xscreensaver
#cinnamon

# fonts
gs-fonts
font-dejavu
font-gnu-freefont-ttf

# A decent default file browser
pcmanfm

# file systems
sshfs-fuse
fuse-exfat
dosfstools
# TODO
#encfs

# web browsers
icecat
# TODO
#elinks
#chromium
#firefox

# terminals
tmux
xterm
xfce4-terminal
gnome-terminal
# TODO
#konsole

# images
feh

# pdf
zathura
zathura-cb
zathura-djvu
zathura-pdf-poppler
zathura-djvu

# DAV, calendar, contacts
khard
khal
vdirsyncer

# email, messaging
offlineimap
mutt
notmuch
gajim

# audio
vorbis-tools
flac
alsa-utils
pavucontrol
mpd
mpd-mpc
ncmpc
abcde
vlc

# writing
texlive
markdown

# notifications
libnotify
dunst
python2-dbus

# misc
qemu
man-pages
the-silver-searcher
# TODO
#pm-utils # power management utils
#espeak
#mlocate

END_OF_PACKAGES
