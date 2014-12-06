#!/bin/sh

df=$DOTFILESWGH

emacs -l $df/emacs/wghconf-package.el -batch -l $df/emacs/install.el
