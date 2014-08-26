#!/bin/bash

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

function mkSundries() {
# Make sundry directories and files so things (especially vim) don't complain
# and stuff that I just like to be there in general
    mkdir -p $HOME/.dotlocal
    touch $HOME/.dotlocal/vimrc
    mkdir -p $HOME/.dotlocal/vimtmp
    mkdir -p $HOME/dl # my default download directory
    mkdir -p $HOME/vsvr
    mkdir -p $HOME/tmp
    mkdir -p $HOME/lscripts
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
else
    echo "usage: linkup.bash < dotfiles | configdir | sundries >"
fi

