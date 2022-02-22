#!/usr/bin/env bash

# nix-shell sets SSL_CERT_FILE to /no-cert-file.crt
unset SSL_CERT_FILE
unset NIX_SSL_CERT_FILE

# Make sure `goal` is on $PATH.
binpath="$XDG_RUNTIME_DIR/$CURRENT_DEV_MODE/bin"
mkdir -p $binpath
ln -s "$DEV_MODE_REACH/scripts/goal-devnet" "$binpath/goal"
PATH=$PATH:$binpath


exec zsh
