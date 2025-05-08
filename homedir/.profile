#!/bin/sh

# NixOS loads this for at least some GUI sessions.  But .xprofile is ALSO loaded for at least some of those sessions...

if [ "$DOT_PROFILE_LOADED" != true ]; then
    export DOT_PROFILE_LOADED=true
    # I think everything is covered by .xprofile here, I just needed this when xprofile was broken due to a syntax error...
    #export WGHHOME=${WGHHOME:-$HOME}
    #export DOTFILESWGH=${DOTFILESWGH:-$WGHHOME/dotfileswgh}
    #source $DOTFILESWGH/env-more-with-mixins.sh
fi
