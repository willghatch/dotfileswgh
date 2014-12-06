#!/usr/bin/zsh

tf=$HOME/.zshrc.pre
if [ -f "$tf" ]
then
    source $tf
fi
tf=$HOME/.bazshrc.pre
if [ -f "$tf" ]
then
    source $tf
fi

HISTSIZE=1000
SAVEHIST=1000

VZSH_REMAP_KEYS_P=true

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

source $HOME/dotfileswgh/bazsh/common.sh
typeset -U PATH
fpath=($fpath $DOTFILESWGH/zsh/completion/)

CDR_DIR=/tmp/$USER/cdr
mkdir -p $CDR_DIR
autoload -Uz chpwd_recent_dirs cdr
autoload -U add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-file $CDR_DIR/recent-dirs

foreground(){
    local b
    zle push-line
    BUFFER="fg"
    zle accept-line
}; zle -N foreground

xclip-to-zsh(){
    BUFFER="${LBUFFER}$(xclip -o -selection clipboard)${RBUFFER}"
}; zle -N xclip-to-zsh

# zaw bookmark file
BOOKMARKFILE=~/.cache/zsh/zaw-bookmarks
# setup for zshmarks plugin
BOOKMARKS_FILE=~/.cache/zsh/zsh-bookmarks
jd(){
    if [[ "ls" = "$1" ]]; then
        showmarks
    elif [[ "add" = "$1" ]]; then
        bookmark "$2"
    else
        jump "$1"
    fi
}

recomp(){
    compinit
    compdump
}

# if cabal is installed, use antigen-hs, since it's so much faster
# ... if not, use regular antigen, because it doesn't need Haskell!
if which cabal 1>/dev/null 2>&1; then
    ANTIGEN_HS_MY=$DOTFILESWGH/zsh/antigen.conf.hs
    source $DOTFILESWGH/external/zsh/antigen-hs/init.zsh
else
    source $DOTFILESWGH/zsh/antigen.conf.zsh
fi

VZSH_MATCHER_STR='m:{a-z-}={A-Z_} m:{b,m,w,v,h,t,n,g,c,r}={0,1,2,3,4,5,6,7,8,9}'
VZSH_ANYWHERE_MATCHER_STR='m:{a-z-}={A-Z_} m:{b,m,w,v,h,t,n,g,c,r}={0,1,2,3,4,5,6,7,8,9} l:|=* r:|=*'
zstyle ':completion:*'  matcher-list $VZSH_MATCHER_STR
zstyle ':completion:vzsh-completion-std-anywhere:*'  matcher-list $VZSH_ANYWHERE_MATCHER_STR
zstyle ':completion:vzsh-completion-history-anywhere:*'  matcher-list $VZSH_ANYWHERE_MATCHER_STR
zstyle ':completion:vzsh-completion-maximal-anywhere:*'  matcher-list $VZSH_ANYWHERE_MATCHER_STR
# separate man pages by section in completion output
zstyle ':completion:*' separate-sections true
# tell what completer is being used, so you can see what's taking a long time
zstyle ':completion:*' show-completer true
# prompt between pages of completions -- MUST BE SET TO ALLOW PAGING
zstyle ':completion:*' list-prompt "%l"

# count / as word separators, so I can kill partial paths
WORDCHARS="${WORDCHARS:s#/#}"

fpath=($fpath $HROOT/build/zsh-completions/src)

if [[ -n "$ZSH_HIGHLIGHT_STYLES" ]]; then
    # Highlighter plugin config
    # Highlighters main
    ZSH_HIGHLIGHT_STYLES[default]="none"
    ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=red,bold"
    ZSH_HIGHLIGHT_STYLES[path]="fg=blue,bold"
    ZSH_HIGHLIGHT_STYLES[path_approx]="fg=cyan"
    ZSH_HIGHLIGHT_STYLES[path_prefix]="fg=cyan"
    ZSH_HIGHLIGHT_STYLES[globbing]="fg=blue,bold,strikethrough"
    ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=yellow"
    ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=blue,bold"
    ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=yellow,bold"
    ZSH_HIGHLIGHT_STYLES[precommand]="fg=green"
    ZSH_HIGHLIGHT_STYLES[command]="fg=green,bold"
    ZSH_HIGHLIGHT_STYLES[hashed-command]="fg=green"
    ZSH_HIGHLIGHT_STYLES[builtin]="fg=green,bold"
    ZSH_HIGHLIGHT_STYLES[function]="fg=green,bold"
    ZSH_HIGHLIGHT_STYLES[alias]="fg=green,bold"
    ZSH_HIGHLIGHT_STYLES[assign]="fg=white,bold"
    ZSH_HIGHLIGHT_STYLES[back-quoted-argument]="fg=magenta"
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]="fg=cyan"
    ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=yellow"
    ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=yellow,bold"
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]="fg=magenta,bold"
    ZSH_HIGHLIGHT_STYLES[single-hyphen-option]="fg=red"
    ZSH_HIGHLIGHT_STYLES[double-hyphen-option]="fg=red"
    # Highlighters root
    ZSH_HIGHLIGHT_STYLES[root]="standout"
    # Highlighters cursor
    ZSH_HIGHLIGHT_STYLES[cursor]="standout"
    # Highlighters brackets
    ZSH_HIGHLIGHT_STYLES[bracket-level-1]="fg=blue,bold"
    ZSH_HIGHLIGHT_STYLES[bracket-level-2]="fg=green,bold"
    ZSH_HIGHLIGHT_STYLES[bracket-level-3]="fg=magenta,bold"
    ZSH_HIGHLIGHT_STYLES[bracket-level-4]="fg=yellow,bold"
    ZSH_HIGHLIGHT_STYLES[bracket-level-5]="fg=cyan,bold"
    ZSH_HIGHLIGHT_STYLES[bracket-error]="fg=red,bold"
    ZSH_HIGHLIGHT_STYLES[cursor-matchingbracket]="standout"
    # Highlighters pattern
    # TODO -- use this one, because it looks cool and helpful, but requires more setup
fi


bindkey -M emacs '^Z' foreground
bindkey -M vicmd 'gp' xclip-to-zsh
# include history substring search commands
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindToMaps '^[c' execute-named-cmd $(bindkey -l)
bindkey -r -M viins '\e'
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode
# o/e keys
bindkey -r -M vicmd 'o'
bindkey -r -M vicmd 'e'
bindkey -M vicmd 'of' vi-find-prev-char
bindkey -M vicmd 'ef' vi-find-next-char
bindkey -M vicmd 'ot' vi-find-prev-char-skip
bindkey -M vicmd 'et' vi-find-next-char-skip
bindkey -M vicmd 'om' vi-rev-repeat-find
bindkey -M vicmd 'em' vi-repeat-find
bindkey -M vicmd 'oe' vi-forward-word-end # TODO - make a backward version
bindkey -M vicmd 'ee' vi-forward-word-end
bindkey -M vicmd 'ol' vi-beginning-of-line
bindkey -M vicmd 'el' vi-end-of-line
bindkey -M vicmd 'oo' vi-open-line-above
bindkey -M vicmd 'eo' vi-open-line-below

bindkey -M vicmd 'f' vi-repeat-find
bindkey -M vicmd 'F' vi-rev-repeat-find
# s map
bindkey -r -M vicmd 's'
bindkey -M vicmd 'sm' vi-set-mark
bindkey -M vicmd 'sg' vi-goto-mark
bindkey -M vicmd 'sG' vi-goto-mark-line
bindkey -r -M vicmd 't'
define-prefix-command spacemap
bindkey -M viins '^[ ' spacemap
bindkey-to-prefix-map spacemap '' zaw
bindkey-to-prefix-map spacemap "r" zaw-history
bindkey-to-prefix-map spacemap "t" zaw-tmux
bindkey-to-prefix-map spacemap "a" zaw-ack
bindkey-to-prefix-map spacemap "f" zaw-git-files
bindkey-to-prefix-map spacemap "s" zaw-git-status
bindkey-to-prefix-map spacemap "o" zaw-open-file
bindkey-to-prefix-map spacemap "p" zaw-process
bindkey-to-prefix-map spacemap "c" zaw-commands
bindkey-to-prefix-map spacemap "d" zaw-cdr
bindkey-to-prefix-map spacemap "f" zaw-functions
bindkey-to-prefix-map spacemap "w" zaw-widgets
bindkey-to-prefix-map spacemap "-" zaw-widgets
# filter-select is the zaw selection mode
# enable smart-case, ^$ for begin/end, and ! to ignore the next word
zstyle ':filter-select' extended-search yes
zstyle ':filter-select' max-lines 15

if [[ "$USER" != "wgh" ]]; then
    MEGAPROMPT_STYLES[username]="%B%F{yellow}"
fi

tf=$DOTFILESWGH/dotlocal/bazshrc
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESWGH/dotlocal/zshrc
if [ -f "$tf" ]
then
    source $tf
fi
unset tf

if [ -x ~/vscripts/motd.sh ]
then
    ~/vscripts/motd.sh
fi

