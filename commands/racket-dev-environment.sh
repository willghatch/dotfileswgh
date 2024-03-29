#!/bin/sh

NIXFILE=$DOTFILESWGH/nixos/racket/racket-env.nix
if test "$1" = "fhs"; then
    NIXFILE=$DOTFILESWGH/nixos/racket/racket-fhs-env.nix
fi

if test -f /etc/NIXOS; then
    exec nix-shell $NIXFILE --pure \
         --keep CURRENT_DEV_PATH \
         --keep CURRENT_DEV_MODE \
         --keep EMACS_DOT_D_PATH \
         --keep LANG \
         --command $DOTFILESWGH/nixos/racket/racket-post-env.sh
else
  echo "Now only supporting NixOS..." 1>&2
  exit 1
fi


