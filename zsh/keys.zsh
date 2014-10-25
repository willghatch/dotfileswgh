HELP_KEY=''

help-help(){
    echo "TODO - put help here" | $PAGER
}
run-help-help(){
    help-help
}
zle -N run-help-help

bindToMaps(){
    # bindToMaps key cmd map1 map2 ...
    local key=$1
    shift
    local cmd=$1
    shift
    for m in $@
    do
        bindkey -M $m $key $cmd 2>/dev/null
    done
}

bindkey-to-prefix-map(){
    # bindkey-to-prefix <prefix> <key> <cmd>
    # TODO - make this work for string '^H' instead of literal ^H
    local name="prefix_command_${1}_map"
    eval "${name}+=( $2 $3 )"
}
define-prefix-command(){
    local mapname=$1
    eval "
    typeset -Ag prefix_command_${mapname}_map
    $mapname(){
        local key
        read -sk 1 key
        local cmd=\$prefix_command_${mapname}_map[\$key]
        zle \$cmd
    }
    zle -N $mapname
    run-help-keys-${mapname}(){
        print -a -C 2 \${(kv)prefix_command_${mapname}_map} | \$PAGER
    }
    zle -N run-help-keys-${mapname}
    bindkey-to-prefix-map '$mapname' $HELP_KEY 'run-help-keys-${mapname}'
    "
}

# nuke viins map, replacing it with the contents of the emacs map
bindkey -A emacs viins

# escapes for command mode
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode

# use viins, which is now emacs-y, but has an out to vicmd mode
bindkey -v

# include history substring search commands
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# completion
define-prefix-command completionkey
bindkey-to-prefix-map completionkey g complete-gnu
bindkey-to-prefix-map completionkey e complete-expand
bindkey-to-prefix-map completionkey n complete-history
bindkey-to-prefix-map completionkey N complete-history-anywhere
bindkey-to-prefix-map completionkey m complete-maximal
bindkey-to-prefix-map completionkey M complete-maximal-anywhere
bindkey-to-prefix-map completionkey s snippet-expand-key
bindkey-to-prefix-map completionkey t complete-tmux
bindkey-to-prefix-map completionkey p insert-last-typed-word
bindkey-to-prefix-map completionkey d insert-datestamp
bindkey -r emacs '^[h'
bindkey -M emacs '^[h' completionkey
# ^i is tab
bindkey -M emacs '^i' complete-std
# ^[[Z is backtab...
bindkey -M emacs '^[[Z' complete-std-anywhere

bindkey -M vicmd 'md' inPlaceMkDirs
bindkey -M vicmd 'g2' jump_after_first_word

bindToMaps '^[c' execute-named-cmd $(bindkey -l)

# Help
bindkey -r emacs '^h'
for m in $(bindkey -l)
do
    bindkey -r $m '^h' 2>/dev/null
done
define-prefix-command helpkey
bindkey-to-prefix-map helpkey g run-help-glob
bindkey-to-prefix-map helpkey b run-help-keys
bindkey-to-prefix-map helpkey s run-help-list-snippets
bindkey-to-prefix-map helpkey c run-help # this pulls up man pages
bindkey-to-prefix-map helpkey h run-help-help
bindToMaps "$HELP_KEY" helpkey $(bindkey -l)

bindkey -M emacs . rationalise-dot
# without this, typing a . aborts incremental history search
bindkey -M isearch . self-insert


