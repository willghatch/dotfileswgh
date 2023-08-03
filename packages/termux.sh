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

binutils

# racket build deps
libffi

)

pkg install "${pkgs[@]}"


