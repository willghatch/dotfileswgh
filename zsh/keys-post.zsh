# include history substring search commands
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# TODO - figure out what to do with ^s ^q flow control nonsense...
bindkey -M emacs '^s' magic-abbrev-expand

# ^i is tab
bindkey -M emacs '^i' wgh-complete
# ^[[Z should be backtab...
bindkey -M emacs '^[[Z' wgh-complete-anywhere
bindkey -M emacs '^[ng' wgh-complete-gnu
bindkey -M emacs '^[ne' wgh-expand
bindkey -M emacs '^[nh' wgh-complete-history

bindkey -M vicmd 'md' inPlaceMkDirs
bindkey -M vicmd 'g2' jump_after_first_word
