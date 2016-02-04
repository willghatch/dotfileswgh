#!/usr/bin/env bash

# Script to put symlinks for dotfiles
# assumes my dotfiles git repo is at $HOME/dotfileswgh


dfDir=dotfileswgh
dpath="$HOME/$dfDir"

dotfiles=(
    bashrc
    zshrc
    vimrc
    gitconfig
    tmux.conf
    muttrc
    makepkg.conf
    inputrc
    pentadactylrc
    xmobarrc
    tigrc
    racketrc
    npmrc
    keynavrc
)

function die() {
    echo $@
    exit 1
}

function mklink() {
    # $1 is the file and $2 is the symlink name
    if [ -e $2 ]
    then
        mv $2 ${2}.prelink
    fi
    mkdir -p $(dirname "$2")
    ln -s $1 $2
}

function linkDotfiles() {
    cd $HOME
    for df in ${dotfiles[@]}
    do
        mklink "$dfDir/$df" ".$df"
    done
}

function linkSubdirs() {
# put a dot as arg 2 if the directory should be dotted in $HOME
    dot=$2
    subdir=$1
    cdir=$dpath/$subdir
    if [ ! -d $cdir ] 
    then
        die "Config dir not found in proper place"
    fi
    cd $cdir
    found=$(find .)
    cd $HOME
    for f in $found
    do
        orig="$dfDir/$subdir/$f"
        if [ -d $orig ]
        then
            mkdir -p "${dot}${subdir}/$f"
        else
            mklink "$HOME/$orig" "${dot}${subdir}/$f"
        fi
    done
}

function mkSundries() {
# Make sundry directories and files so things (especially vim) don't complain
# and stuff that I just like to be there in general
    mkdir -p $HOME/.cache/vimtmp
    # the cache could hold sensitive stuff
    chmod 700 $HOME/.cache
    mkdir -p $HOME/dl # my default download directory
    mkdir -p $HOME/vserv-mount
    ln -s vserv-mount/vsvr $HOME/vsvr
    mkdir -p $HOME/tmp
    mkdir -p $HOME/.themes
    mkdir -p $HOME/.config
    mkdir -p $HOME/.xmonad
    mkdir -p $HOME/.elinks
    mkdir -p $HOME/.themes
    mkdir -p $HOME/.prifs
    mkdir -p $HOME/prifs
    chmod 700 $HOME/.prifs $HOME/prifs
    mkdir -p $dpath/dotlocal/firefox-default-profile/extensions
    # dirs for mpd
    mkdir -p $HOME/.cache/mpd/playlists
    mkdir -p $HOME/musc-local
}

function linkSundries() {
    mkSundries
    mklink $dpath/external/gtk/gtk-adb $HOME/.themes/gtk-adb
    mklink $dpath/xmonad.hs $HOME/.xmonad/xmonad.hs
    mklink $dpath/elinks/elinks.conf $HOME/.elinks/elinks.conf
    mkdir -p $HOME/.mozilla/firefox/profiles.ini
    mklink $dpath/firefox-profiles.ini $HOME/.mozilla/firefox/profiles.ini
}

#######################################

if [ "s-$1" = "s-dotfiles" ]
then
    linkDotfiles
elif [ "s-$1" = "s-configdir" ]
then
    linkSubdirs config .
    linkSubdirs local .
    # this *really* should be in ~/.local/share/icons/... but apparently some icons
    # are required to be in ~/.icons/
    linkSubdirs icons .
    linkSubdirs ssh .
elif [ "s-$1" = "s-sundries" ]
then
    mkSundries
elif [ "s-$1" = "s-linksundries" ]
then
    linkSundries
elif [ "s-$1" = "s-all" ]
then
    mkSundries
    linkDotfiles
    linkSubdirs config .
    linkSubdirs local .
    linkSundries
else
    echo "usage: linkup.bash < dotfiles | configdir | sundries | linksundries | all >"
fi

