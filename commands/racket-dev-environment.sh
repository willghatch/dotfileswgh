#!/bin/sh

# HACK - try to get libcrypto.so by any means necessary...
# Racket doesn't get it from pkgconfig, it only dynamically loads the shared object file.
# I need to remember to update this link every time I update my pinned racket dev packages...
LD_LIBRARY_PATH=$HOME/mk/racket-dev-env-openssl/lib:$LD_LIBRARY_PATH

if test -f /etc/NIXOS; then
  exec nix-shell $DOTFILESWGH/nixos/racket-env.nix --pure --keep CURRENT_DEV_PATH --keep CURRENT_DEV_MODE --keep LD_LIBRARY_PATH --command zsh
else
  echo "Now only supporting NixOS..." 1>&2
  exit 1
fi
