#!/usr/bin/env zsh
# abbreviations... a mix of how grml does it and where grml got it from...

typeset -Ag abbreviations
abbreviations=(
    "l"    "less "
    "tl"   "| less "
    "g"    "grep "
    "tg"   "| grep "
    "gl"    "grep -l"
    "tgl"   "| grep -l"
    "gL"    "grep -L"
    "tgL"   "| grep -L"
    "gv"   "grep -v "
    "tgv"  "| grep -v "
    "eg"   "egrep "
    "teg"  "| egrep "
    "fg"   "fgrep "
    "tfg"  "| fgrep "
    "fgv"  "fgrep -v "
    "tfgv" "| fgrep -v "
    "ag"   "agrep "
    "tag"  "| agrep "
    "ta"   "| ag "
    "p"    "$PAGER "
    "tp"   "| $PAGER "
    "h"    "head "
    "th"   "| head "
    "t"    "tail "
    "tt"   "| tail "
    "s"    "sort "
    "ts"   "| sort "
    "v"    "${VISUAL:-${EDITOR}} "
    "tv"   "| ${VISUAL:-${EDITOR}} "
    "tc"   "| cut "
    "tu"   "| uniq "
    "tx"   "| xargs "
)

magic-abbrev-expand() {
    emulate -L zsh
    setopt extendedglob
    local MATCH

    LBUFFER=${LBUFFER%%(#m)[.\-+:|_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
}

zle -N magic-abbrev-expand

