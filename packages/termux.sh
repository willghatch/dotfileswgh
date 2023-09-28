#!/data/data/com.termux/files/usr/bin/env bash

pkgs=(
git
vim
emacs
zsh
grep
termux-api
ncurses-utils
openssh
tmux
#racket
python
which
htop
rsync

binutils
mount-utils

# racket build deps
libffi

)

pkg install "${pkgs[@]}"


