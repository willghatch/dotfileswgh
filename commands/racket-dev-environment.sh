#!/bin/sh

# Before I switched to NixOS, my Racket build environment scripts just set some environment variables.
# Then after I switched to NixOS this made a fancy nix shell environment.  But that was awful.
# NixOS is dogmatic about NOT allowing development in the system environment.  What nonsense.  After several iterations of different build environments that were all unsatisfactory in one way or another, I'm going to just try working around NixOS nonsense and build in the system environment, just with a few environment variables set.
# It will probably still end in tears.

if test -f /etc/NIXOS; then
  #exec nix-shell $DOTFILESWGH/nixos/racket-env.nix --pure --keep CURRENT_DEV_PATH --keep CURRENT_DEV_MODE

  # Even with all *.dev, etc, packages installed in my system packages, nixos still doesn't link up the include directories, so I have to futz with include paths.
  export PKG_CONFIG_PATH=/run/current-system/sw/lib/pkgconfig/
  h_uuidpath=`pkg-config --cflags uuid | sed s/-I//`
  export C_INCLUDE_PATH="${h_uuidpath:0:-4}"
  l_uuidpath=`pkg-config --libs uuid | sed s/-L// | sed "s/ -luuid//"`
  export LIBRARY_PATH="$l_uuidpath"
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/run/current-system/sw/lib
  exec bash
else
  echo "Now only supporting NixOS..." 1>&2
  exit 1
fi


