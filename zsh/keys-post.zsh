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

# include history substring search commands
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# TODO - figure out what to do with ^s ^q flow control nonsense...
bindkey -M emacs '^s' magic-abbrev-expand

# ^i is tab
bindkey -M emacs '^i' wgh-complete
# ^[[Z should be backtab...
bindkey -M emacs '^[[Z' wgh-complete-anywhere
bindkey -r emacs '^[h'
bindkey -M emacs '^[hg' wgh-complete-gnu
bindkey -M emacs '^[he' wgh-expand
bindkey -M emacs '^[hn' wgh-complete-history
bindkey -M emacs '^[hN' wgh-complete-history-anywhere
bindkey -M emacs '^[hs' magic-abbrev-expand
bindkey -M emacs '^[ht' wgh-complete-screen
bindkey -M emacs '^[hp' insert-last-typed-word
bindkey -M emacs '^[hd' insert-datestamp
bindkey -M emacs '^[hm' wgh-complete-maximal
bindkey -M emacs '^[hM' wgh-complete-maximal-anywhere

bindkey -M vicmd 'md' inPlaceMkDirs
bindkey -M vicmd 'g2' jump_after_first_word

bindkey -M emacs '^[c' execute-named-cmd

bindkey -r emacs '^h'
for m in $(bindkey -l)
do
    bindkey -r $m '^h' 2>/dev/null
done
bindToMaps '^hh' run-help $(bindkey -l)
bindToMaps '^ha' run-help-show-abbrev $(bindkey -l)
bindToMaps '^hg' run-help-glob $(bindkey -l)
bindToMaps '^hb' run-help-keys $(bindkey -l)



