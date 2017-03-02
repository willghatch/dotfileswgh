#!/usr/bin/env bash

# install the packages I most use from the AUR

pa (){
    pacaur -S --needed --noconfirm --noedit "$@"
}

pa cower
pa pacaur

pa xkbset
#pa trayer-srg

# needed for abcde to get metadata...
pa perl-musicbrainz-discid perl-webservice-musicbrainz

