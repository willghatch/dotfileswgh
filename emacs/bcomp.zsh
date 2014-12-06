#!/usr/bin/env zsh

# Quick script to recursively byte-compile all .el files

df=$DOTFILESWGH

emacs -l $df/emacs/def.el -batch -f batch-byte-compile $df/**/*.el ~/.emacs.d/**/*.el

