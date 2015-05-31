#!/usr/bin/env bash

pa (){
    pacaur -S --needed --noconfirm --noedit "$@"
}

pa cower
pa pacaur

pa xkbset
pa ledmon
#pa trayer-srg

