#!/usr/bin/env bash

## Echo packages for consumption by apt-get install

i(){
    apt-get install $@
}

i git
i vim
i emacs-nox
i zsh
i tmux
i curl
i sshfs
i silversearcher-ag
i dnsutils
i nmap
i python3
i tig
i mutt
i offlineimap
i htop
i racket

