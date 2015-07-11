#!/usr/bin/env bash


if [[ ! -n $(pacman -Qdt) ]]; then
    echo "No orphans to remove."
else
    pacman -Rns $(pacman -Qdtq)
fi

