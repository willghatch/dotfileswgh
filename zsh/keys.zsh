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
# TODO - figure out what to do with ^s ^q flow control nonsense...
bindkey -M emacs '^s' magic-abbrev-expand

# completion
define-prefix-command completionkey
bindkey-to-prefix-map completionkey g wgh-complete-gnu
bindkey-to-prefix-map completionkey e wgh-expand
bindkey-to-prefix-map completionkey n wgh-complete-history
bindkey-to-prefix-map completionkey N wgh-complete-history-anywhere
bindkey-to-prefix-map completionkey s magic-abbrev-expand
bindkey-to-prefix-map completionkey t wgh-complete-screen
bindkey-to-prefix-map completionkey p insert-last-typed-word
bindkey-to-prefix-map completionkey d insert-datestamp
bindkey-to-prefix-map completionkey m wgh-complete-maximal
bindkey-to-prefix-map completionkey M wgh-complete-maximal-anywhere
bindkey -r emacs '^[h'
bindkey -M emacs '^[h' completionkey
# ^i is tab
bindkey -M emacs '^i' wgh-complete
# ^[[Z should be backtab...
bindkey -M emacs '^[[Z' wgh-complete-anywhere

bindkey -M vicmd 'md' inPlaceMkDirs
bindkey -M vicmd 'g2' jump_after_first_word

bindkey -M emacs '^[c' execute-named-cmd

# Help
bindkey -r emacs '^h'
for m in $(bindkey -l)
do
    bindkey -r $m '^h' 2>/dev/null
done
define-prefix-command helpkey
bindkey-to-prefix-map helpkey g run-help-glob
bindkey-to-prefix-map helpkey b run-help-keys
bindkey-to-prefix-map helpkey a run-help-show-abbrev
bindkey-to-prefix-map helpkey c run-help
bindkey-to-prefix-map helpkey h run-help-help
bindToMaps "$HELP_KEY" helpkey $(bindkey -l)
#bindToMaps '^ha' run-help-show-abbrev $(bindkey -l)
#bindToMaps '^hg' run-help-glob $(bindkey -l)
#bindToMaps '^hb' run-help-keys $(bindkey -l)



