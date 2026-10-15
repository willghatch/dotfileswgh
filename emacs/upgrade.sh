#!/usr/bin/env sh

df=$DOTFILESWGH

emacs --init-directory "${EMACS_DOT_D_PATH:-$DOTFILESWGH_DOTLOCAL/emacs.d}" -l $df/emacs/package-conf.el -batch -l $df/emacs/upgrade.el
