#!/bin/sh

df=$DOTFILESWGH

emacs -l $df/emacs/package-conf.el -batch -l $df/emacs/install.el
