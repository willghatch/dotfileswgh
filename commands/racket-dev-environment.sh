#!/bin/sh

# HACK - try to get libcrypto.so by any means necessary...
# Racket doesn't get it from pkgconfig, it only dynamically loads the shared object file.
# I need to remember to update this link every time I update my pinned racket dev packages...
# Needed libs: libcrypto.so, libsqlite3.so, libglib.so, libfontconfig.so, libcairo.so, libpango.so, libjpeg.so, libreadline.so, libedit.so
for d in $HOME/mk/racket-dev-lib/*/lib; do
    LD_LIBRARY_PATH=$d:$LD_LIBRARY_PATH
done

if test -f /etc/NIXOS; then
  exec nix-shell $DOTFILESWGH/nixos/racket-env.nix --pure --keep LD_LIBRARY_PATH --keep CURRENT_DEV_PATH --keep CURRENT_DEV_MODE  --command zsh
else
  echo "Now only supporting NixOS..." 1>&2
  exit 1
fi
