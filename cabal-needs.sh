#!/bin/sh

# these are because apparently cabal can't figure out build dependencies...
cabal install happy # for c2hs... and lots of stuff
cabal install alex # just in case

cabal install xmonad
cabal install xmonad-contrib

cabal install c2hs # for alsa stuff
cabal install alsa-core
cabal install alsa-mixer
cabal install libmpd

cabal install xmobar --flags="with_alsa,with_mpd"

# for antigen-hs
cabal install shelly

xmonad --recompile

