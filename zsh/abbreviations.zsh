#!/usr/bin/env zsh
# abbreviations... a mix of how grml does it and where grml got it from...

typeset -Ag abbreviations
abbreviations=(
    "l"    "| less "
    "g"    "| grep "
    "gv"   "| grep -v "
    "eg"   "| egrep "
    "fg"   "| fgrep "
    "fgv"  "| fgrep -v "
    "ag"   "| agrep "
    "p"    "| $PAGER "
    "h"    "| head "
    "t"    "| tail "
    "s"    "| sort "
    "v"    "| ${VISUAL:-${EDITOR}} "
    "c"    "| wc "
    "x"    "| xargs "
)

magic-abbrev-expand() {
    emulate -L zsh
    setopt extendedglob
    local MATCH

    LBUFFER=${LBUFFER%%(#m)[.\-+:|_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
}

zle -N magic-abbrev-expand

