#!/usr/bin/env sh

df=$DOTFILESWGH

emacs -l $df/emacs/package-conf.el -batch -l $df/emacs/upgrade.el
