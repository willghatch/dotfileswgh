#!/usr/bin/env bash

# install the packages I most use from the AUR

pa (){
    pacaur -S --needed --noconfirm --noedit "$@"
}

pa cower
pa pacaur

pa xkbset
#pa trayer-srg

# like `git svn` for mercurial
pa git-remote-hg-git

# needed for abcde to get metadata...
pa perl-musicbrainz-discid perl-webservice-musicbrainz

#pa vdirsyncer
#pa khard
#pa khal
#pa todoman

