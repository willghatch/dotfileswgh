#!/usr/bin/zsh
# abbreviations... a mix of how grml does it and where grml got it from...

typeset -Ag abbreviations
abbreviations=(
    "Im"    "| more"
    "Ia"    "| awk"
    "Ig"    "| grep"
    "Ieg"   "| egrep"
    "Iag"   "| agrep"
    "Igr"   "| groff -s -p -t -e -Tlatin1 -mandoc"
    "Ip"    "| $PAGER"
    "Ih"    "| head"
    "Ik"    "| keep"
    "It"    "| tail"
    "Is"    "| sort"
    "Iv"    "| ${VISUAL:-${EDITOR}}"
    "Iw"    "| wc"
    "Ix"    "| xargs"
)

magic-abbrev-expand() {
    emulate -L zsh
    setopt extendedglob
    local MATCH

    LBUFFER=${LBUFFER%%(#m)[.\-+:|_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
}

zle -N magic-abbrev-expand

