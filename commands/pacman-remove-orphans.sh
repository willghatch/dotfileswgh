#!/usr/bin/env bash

# Remove all orphan packages.  Not recursive -- needs to be re-run to
# be sure to remove orphans' newly orphaned children.

if [[ ! -n $(pacman -Qdt) ]]; then
    echo "No orphans to remove."
else
    pacman -Rns $(pacman -Qdtq)
fi

