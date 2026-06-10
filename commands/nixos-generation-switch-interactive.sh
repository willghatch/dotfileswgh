#!/usr/bin/env bash
set -euo pipefail

nix-env --list-generations --profile /nix/var/nix/profiles/system

printf '\nGeneration number to switch to: '
read -r gen

if [[ ! "$gen" =~ ^[0-9]+$ ]]; then
    echo "Error: '$gen' is not a valid generation number." >&2
    exit 1
fi

nix-env --switch-generation "$gen" --profile /nix/var/nix/profiles/system
/nix/var/nix/profiles/system/bin/switch-to-configuration boot
/nix/var/nix/profiles/system/bin/switch-to-configuration switch
