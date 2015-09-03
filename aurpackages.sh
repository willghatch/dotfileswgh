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

