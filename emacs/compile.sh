#!/usr/bin/env bash

# Quick script to recursively byte-compile all .el files

df=$DOTFILESWGH

files=$(ls *.el | grep -v "test-" | grep -v straight-lockfile)
external_files=$(find $DOTFILESWGH/external/emacs -type f -name '*.el')

#emacs -l $df/emacs/def.el -batch -f batch-byte-compile $df/emacs/*.el
#emacs -l $df/emacs/def.el -batch -f batch-native-compile $df/emacs/{def,*-conf,keys,init-helpers,estate-core,estate-vim-like-states,scratch-message,scribble-funcs,sensitive-mode,text-object-stuff,tree-walk-smartparens-integration,tree-walk,vfuncs,wgh-theme}.el

emacs -l $DOTFILESWGH/emacs/def.el -batch --eval "(with-demoted-errors (batch-native-compile))" $files $external_files
