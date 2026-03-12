#!/usr/bin/env bash

if [[ "$@" =~ "--help" || "$#" = 0 ]]; then
    cat $(readlink -f "$0")
    exit 0
fi

certfile="$1"

openssl x509 -in $certfile -noout -sha256 -fingerprint
openssl x509 -in $certfile -noout -sha1 -fingerprint
openssl x509 -in $certfile -noout -md5 -fingerprint
