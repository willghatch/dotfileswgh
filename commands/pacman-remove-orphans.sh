#!/usr/bin/env bash

# Remove all orphan packages.  Not recursive -- needs to be re-run to
# be sure to remove orphans' newly orphaned children.

if [[ "$@" =~ "--help" ]]; then
    echo "Usage: $0"
    echo "Remove all orphan packages via pacman. Re-run to remove transitively orphaned packages."
    exit 0
fi

if [[ ! -n $(pacman -Qdt) ]]; then
    echo "No orphans to remove."
else
    pacman -Rns $(pacman -Qdtq)
fi

