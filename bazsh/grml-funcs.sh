#!/bin/sh

################## some functions taken from grml zsh config, but made to work with bash too

# mkdir and cd
mkcd() {
    if [[ -z "$1" ]]; then
        printf 'usage: mkcd <new-directory>\n'
        return 1;
    fi
    if [[ ! -d "$1" ]]; then
        mkdir -p "$1"
    else
        printf '`%s'\'' already exists: cd-ing.\n' "$1"
    fi
    builtin cd "$1"
}

# make temp dir and cd to it
cdt() {
    local t
    t=$(mktemp -d bazsh-XXXXX)
    echo "$t"
    builtin cd "$t"
}


