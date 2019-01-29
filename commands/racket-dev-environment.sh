#!/bin/sh

if test -f /etc/NIXOS; then
  exec nix-shell $DOTFILESWGH/nixos/racket-env.nix --pure --keep CURRENT_DEV_PATH --keep CURRENT_DEV_MODE
else
  echo "Now only supporting NixOS..." 1>&2
  exit 1
fi
