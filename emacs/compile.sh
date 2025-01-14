#!/usr/bin/env bash

# Quick script to recursively byte-compile all .el files.  Or maybe native compile them.
# But after some trials and tribulations, I'm settling on using this only for compiling things in my emacs dir, not even submodules.  I'll use straight.el to compile all packages.

df=$DOTFILESWGH

files=$(ls *.el | grep -v "test-" | grep -v straight-lockfile)
external_files=$(find $DOTFILESWGH/external/emacs -type f -name '*.el')

#emacs -l $df/emacs/def.el -batch -f batch-byte-compile $df/emacs/*.el

# This is not getting all of the files, particularly it is missing many things in $DOTFILESWGH/external/emacs.

compile-single() {
    # For things outside of my dotfiles, that don't implicitly depend on my sloppy emacs code, don't load def.el.
    emacs -l $DOTFILESWGH/emacs/dotfileswgh-env-conf.el -batch --eval "(with-demoted-errors (batch-native-compile))" "$1"
}

#for f in $external_files; do
#    compile-single "$f"
#done


#emacs -l $DOTFILESWGH/emacs/dotfileswgh-env-conf.el -batch --eval "(with-demoted-errors (batch-native-compile))" $external_files

# batch compile all of my non-packaged elisp together
emacs -l $DOTFILESWGH/emacs/def.el -batch --eval "(with-demoted-errors (batch-native-compile))" $files
