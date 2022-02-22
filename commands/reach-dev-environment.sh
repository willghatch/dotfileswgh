#!/bin/sh

NIXFILE=$DOTFILESWGH/nixos/reach-env.nix
if test "$1" = "fhs"; then
    NIXFILE=$DOTFILESWGH/nixos/reach-fhs-env.nix
fi

if test -f /etc/NIXOS; then
    exec nix-shell $NIXFILE --pure \
         --keep CURRENT_DEV_PATH \
         --keep CURRENT_DEV_MODE \
         --keep EMACS_DOT_D_PATH \
         --keep LANG \
         --command $DOTFILESWGH/nixos/reach-post-env.sh
else
  echo "Now only supporting NixOS..." 1>&2
  exit 1
fi


