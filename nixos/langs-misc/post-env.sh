#!/usr/bin/env bash

# nix-shell doesn't add LD_LIBRARY_PATH, which means `raco pkg` and other things fail, because Racket can't find libcrypto.so from the openssl package.  This is why I previously used FHSUserEnv.  But it does add NIX_TARGET_LDFLAGS, which has the needed info...

# This was previously NIX_TARGET_LDFLAGS but has changed to NIX_LDFLAGS_FOR_TARGET...
export LD_LIBRARY_PATH="$(env | grep NIX_LDFLAGS_FOR_TARGET | sed "s/ -L/:/g" | sed s/NIX_LDFLAGS_FOR_TARGET=://):/lib"

# nix-shell sets SSL_CERT_FILE to /no-cert-file.crt
unset SSL_CERT_FILE
unset NIX_SSL_CERT_FILE

export CURRENT_DEV_MODE=langs-misc
export EMACS_DOT_D_PATH=$DOTFILESWGH_DOTLOCAL/emacs.d_langs-misc


#exec zsh
exec bash
