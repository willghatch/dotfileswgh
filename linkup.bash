#!/usr/bin/env bash

# Script to put symlinks for dotfiles
# assumes my dotfiles git repo is at $HOME/dotfileswgh


dfDir=dotfileswgh

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
)

function die() {
    echo $@
    exit 1
}

function mklink() {
    # $1 is the file and $2 is the symlink name
    if [ -e $2 ]
    then
        mv $2 ${2}.old
    fi
    ln -s $1 $2
}

function linkDotfiles() {
    cd $HOME
    for df in ${dotfiles[@]}
    do
        mklink "$dfDir/$df" ".$df"
    done
}

function linkConfigSubdirs() {
    cdir=$HOME/$dfDir/config
    if [ ! -d $cdir ] 
    then
        die "Config dir not found in proper place"
    fi
    cd $cdir
    found=$(find .)
    cd $HOME
    for f in $found
    do
        orig="$dfDir/config/$f"
        if [ -d $orig ]
        then
            mkdir -p ".config/$f"
        else
            mklink "$HOME/$orig" ".config/$f"
        fi
    done
}

function linkSundries() {
    # link in themes!
    mkdir -p $HOME/.themes
    cd $HOME/.themes
    ln -s $HOME/dotfileswgh/external/gtk/gtk-adb $HOME/.themes/
    ln -s $HOME/dotfileswgh/xmonad.hs $HOME/.xmonad/xmonad.hs
    ln -s $HOME/dotfileswgh/elinks/elinks.conf $HOME/.elinks/elinks.conf
}

function mkSundries() {
# Make sundry directories and files so things (especially vim) don't complain
# and stuff that I just like to be there in general
    mkdir -p $HOME/.cache/vimtmp
    mkdir -p $HOME/dl # my default download directory
    mkdir -p $HOME/vsvr
    mkdir -p $HOME/tmp
    mkdir -p $HOME/.themes
    mkdir -p $HOME/.config
    mkdir -p $HOME/.xmonad
    mkdir -p $HOME/.elinks
}

#######################################

if [ "s-$1" = "s-dotfiles" ]
then
    linkDotfiles
elif [ "s-$1" = "s-configdir" ]
then
    linkConfigSubdirs
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
    linkConfigSubdirs
    linkSundries
else
    echo "usage: linkup.bash < dotfiles | configdir | sundries | linksundries | all >"
fi

