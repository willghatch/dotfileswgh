#!/usr/bin/env bash

# install the packages I most use from the AUR

pa (){
    pacaur -S --needed --noconfirm --noedit "$@"
}

pa cower
pa pacaur

pa xkbset
pa ledmon
#pa trayer-srg
pa khal
pa khard-git

# I don't actually care, but hey, let's quiet those gtk errors
pa gtk-engine-unico

