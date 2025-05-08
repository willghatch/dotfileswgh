#!/bin/sh

# NixOS loads this for at least some GUI sessions.

if [ "$DOT_PROFILE_LOADED" != true ]; then
    export DOT_PROFILE_LOADED=true
    export WGHHOME=${WGHHOME:-$HOME}
    export DOTFILESWGH=${DOTFILESWGH:-$WGHHOME/dotfileswgh}
    source $DOTFILESWGH/env-more-with-mixins.sh
fi
