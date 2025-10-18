#!/data/data/com.termux/files/usr/bin/env bash

pkgs=(
git
vim
emacs
zsh
grep
jq
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

termux-api
termux-exec # allows /bin/sh and /usr/bin/env for scripts rather than needing to fuss with paths.  Should already be installed on modern termux.
tudo # sudo-like interface for setting up the LD_PRELOAD stuff from termux-exec in environments where it is not automatically set up, such as when running from the RUN_COMMAND intent

)

pkg install "${pkgs[@]}"


