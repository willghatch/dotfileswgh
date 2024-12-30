#!/usr/bin/env zsh

# Quick script to recursively byte-compile all .el files

df=$DOTFILESWGH

#emacs -l $df/emacs/def.el -batch -f batch-byte-compile $df/emacs/*.el
#emacs -l $df/emacs/def.el -batch -f batch-native-compile $df/emacs/{def,*-conf,keys,init-helpers,estate-core,estate-vim-like-states,scratch-message,scribble-funcs,sensitive-mode,text-object-stuff,tree-walk-smartparens-integration,tree-walk,vfuncs,wgh-theme}.el

