#!/usr/bin/env bash

# nix-shell doesn't add LD_LIBRARY_PATH, which means `raco pkg` and other things fail, because Racket can't find libcrypto.so from the openssl package.  This is why I previously used FHSUserEnv.  But it does add NIX_TARGET_LDFLAGS, which has the needed info...

export LD_LIBRARY_PATH="$(env | grep NIX_TARGET_LDFLAGS | sed "s/ -L/:/g" | sed s/NIX_TARGET_LDFLAGS=://):/lib"

# nix-shell sets SSL_CERT_FILE to /no-cert-file.crt
unset SSL_CERT_FILE
unset NIX_SSL_CERT_FILE

exec zsh
