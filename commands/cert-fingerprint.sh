#!/usr/bin/env bash

certfile="$1"

openssl x509 -in $certfile -noout -sha256 -fingerprint
openssl x509 -in $certfile -noout -sha1 -fingerprint
openssl x509 -in $certfile -noout -md5 -fingerprint
