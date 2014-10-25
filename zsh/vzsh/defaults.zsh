# Default settings for necessary vzsh and zsh variables and some needed functions
# user-defined variables are all left alone

if [[ -z "$VZSH_CACHE" ]]; then
    VZSH_CACHE=$HOME/.cache/vzsh
fi
if [[ ! -d $VZSH_CACHE ]]; then
    mkdir -p $VZSH_CACHE
fi
if [[ -z "$HISTFILE" ]]; then
    HISTFILE=$VZSH_CACHE/history
fi
if [[ -z "$HISTSIZE" ]]; then
    HISTSIZE=1000
fi
if [[ -z "$SAVEHIST" ]]; then
    SAVEHIST=1000
fi
if [[ -z "$PAGER" ]]; then
    PAGER=less
fi
if [[ -z "$COMPINSTALL_FILE" ]]; then
    COMPINSTALL_FILE=$VZSH_CACHE/compinstall
fi
if [[ -z "$VZSH_RECENT_DIRS_DIR" ]]; then
    VZSH_RECENT_DIRS_DIR=$VZSH_CACHE/recent-dirs
fi
if [[ ! -d $VZSH_RECENT_DIRS_DIR ]]; then
    mkdir -p $VZSH_RECENT_DIRS_DIR
fi

if [[ -z "$ZLE_LINE_INIT_FUNCS" ]]; then
    typeset -ag ZLE_LINE_INIT_FUNCS
fi
zle-line-init(){
    for func in $ZLE_LINE_INIT_FUNCS; do
        $func
    done
}
zle -N zle-line-init

if [[ -z "$ZLE_KEYMAP_SELECT_FUNCS" ]]; then
    typeset -ag ZLE_KEYMAP_SELECT_FUNCS
fi
zle-keymap-select(){
    CUR_KEYMAP=$KEYMAP
    for func in $ZLE_KEYMAP_SELECT_FUNCS; do
        $func
    done
}
zle -N zle-keymap-select

help-help(){
    echo "TODO - put help here" | $PAGER
}
run-help-help(){
    help-help
}
zle -N run-help-help

