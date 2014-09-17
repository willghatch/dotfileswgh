#!/usr/bin/env zsh
# abbreviations... a mix of how grml does it and where grml got it from...

typeset -Ag abbreviations
abbreviations=(
    "l"    "less "
    "-l"   "| less "
    "g"    "grep "
    "-g"   "| grep "
    "gl"    "grep -l"
    "-gl"   "| grep -l"
    "gL"    "grep -L"
    "-gL"   "| grep -L"
    "gv"   "grep -v "
    "-gv"  "| grep -v "
    "eg"   "egrep "
    "-eg"  "| egrep "
    "fg"   "fgrep "
    "-fg"  "| fgrep "
    "fgv"  "fgrep -v "
    "-fgv" "| fgrep -v "
    "ag"   "agrep "
    "-ag"  "| agrep "
    "p"    "$PAGER "
    "-p"   "| $PAGER "
    "h"    "head "
    "-h"   "| head "
    "t"    "tail "
    "-t"   "| tail "
    "s"    "sort "
    "-s"   "| sort "
    "v"    "${VISUAL:-${EDITOR}} "
    "-v"   "| ${VISUAL:-${EDITOR}} "
    "c"    "wc "
    "-c"   "| wc "
    "-x"   "| xargs "
)

magic-abbrev-expand() {
    emulate -L zsh
    setopt extendedglob
    local MATCH

    LBUFFER=${LBUFFER%%(#m)[.\-+:|_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
}

zle -N magic-abbrev-expand

