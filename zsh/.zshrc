
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

if [[ -z "$WGHHOME" ]]; then
    export WGHHOME=$HOME # I want to be able to reference this, and when it's
    #not true(IE when I use su), I'll set this elsewhere
fi

COMPDUMPFILE=$HOME/.cache/zshcompdump
ZSH_COMPDUMP=$COMPDUMPFILE
# autoload compinit to get the compdef function
# compinit -d <dumpfile> [ -u to use insecure dirs, -i to ignore SILENTLY, -C to skip security check ]
COMPINIT_COMMAND="compinit -d $COMPDUMPFILE -i"
# shadow compinit to prevent plugins from calling it, because it is SLOW!
compdef(){}
compinit(){}

HISTSIZE=1000
SAVEHIST=1000

HELP_KEY="\033^H"
VZSH_REMAP_KEYS_P=true

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

# source fasd before bazsh, because they add some aliases that I will overwrite
eval "$(fasd --init auto >/dev/null 2>&1)"

source $WGHHOME/dotfileswgh/bazshrc
typeset -U PATH
fpath=($fpath /run/current-system/sw/share/zsh/site-functions)
fpath=($fpath $DOTFILESWGH/zsh/completion/)

xclip-to-zsh(){
    BUFFER="${LBUFFER}$(xclip -o -selection clipboard)${RBUFFER}"
}; zle -N xclip-to-zsh

_search-forward-current-command(){
    pat="$1"
    for (( i=1; i <= ${#RBUFFER}; i++)); do
        if [[ "${RBUFFER:$i:${#pat}}" = "$pat" ]]; then
            CURSOR=$(($CURSOR + $i))
            break
        fi
    done
}; zle -N _search-forward-current-command
_search-backward-current-command(){
    pat="$1"
    for (( i=$((${#LBUFFER} - 1)); i>=0; i--)); do
        if [[ "${BUFFER:$i:${#pat}}" = "$pat" ]]; then
            CURSOR=$i
            break
        fi
    done
}; zle -N _search-backward-current-command

search-forward-current-command(){
    autoload -Uz read-from-minibuffer
    read-from-minibuffer "/"
    _last_search_current_command="$REPLY"
    _last_search_current_command_dir="f"
    zle _search-forward-current-command "$_last_search_current_command"
}; zle -N search-forward-current-command
search-backward-current-command(){
    autoload -Uz read-from-minibuffer
    read-from-minibuffer "?"
    _last_search_current_command="$REPLY"
    _last_search_current_command_dir="b"
    zle _search-backward-current-command "$_last_search_current_command"
}; zle -N search-backward-current-command

search-forward-current-command-repeat(){
    if [[ "$_last_search_current_command_dir" = "f" ]]; then
        zle _search-forward-current-command "$_last_search_current_command"
    else
        zle _search-backward-current-command "$_last_search_current_command"
    fi
}; zle -N search-forward-current-command-repeat
search-backward-current-command-repeat(){
    if [[ "$_last_search_current_command_dir" = "f" ]]; then
        zle _search-backward-current-command "$_last_search_current_command"
    else
        zle _search-forward-current-command "$_last_search_current_command"
    fi
}; zle -N search-backward-current-command-repeat

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

compinit-widget(){
    ${=COMPINIT_COMMAND}
}; zle -N compinit-widget

ZAW_MPC_COMMAND="mpc --port 6637"

# TODO - see where lazy sourcing can speed things up
lazy_source () {
    eval "$1 () { [ -f $2 ] && source $2 && $1 \$@ }"
}

ZGEN_AUTOLOAD_COMPINIT=false
ZGEN_DIR=$DOTFILESWGH/dotlocal/zsh/zgen
lazy_source zgen $DOTFILESWGH/external/zsh/zgen/zgen.zsh
#if ! zgen saved; then
if ! source "$ZGEN_DIR/init.zsh"; then
    echo "zgen not set up -- cloning repos"
    zgen load zsh-users/zsh-history-substring-search
    #zgen load hchbaw/opp.zsh
    zgen load willghatch/zsh-cdr
    zgen load zsh-users/zaw
    zgen load willghatch/zsh-zaw-mpd
    zgen load willghatch/zsh-zaw-todoman
    zgen load willghatch/zsh-saneopt
    zgen load willghatch/zsh-hooks
    zgen load willghatch/zsh-megaprompt
    zgen load willghatch/zsh-snippets
    zgen load willghatch/zsh-grml-funcs
    zgen load willghatch/vzsh
    #zgen load termoshtt/zaw-systemd
    #zgen load junkblocker/calibre-zaw-source
    #zgen load tarrasch/zsh-colors
    #zgen load tarrasch/zsh-bd
    #zgen load tarrasch/zsh-functional
    #zgen load jocelynmallon/zshmarks
    #zgen load skx/sysadmin-util
    #zgen load ehamberg/zsh-cabal-completion
    zgen load zsh-users/zsh-syntax-highlighting
    zgen load racket/shell-completion
    # this one doesn't seem to work...
    #zgen load RobSis/zsh-reentry-hook

    echo "saving zgen init file"
    zgen save
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
# count other stuff as word characters, because it always messes me up
WORDCHARS="${WORDCHARS:s#/#}+"

fpath=($fpath $HROOT/build/zsh-completions/src)

updateColorStyle(){
    # TODO - I should specify all styles for both light and dark so I get consistent colors either way.
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
        ZSH_HIGHLIGHT_STYLES[cursor]="underline"
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
    if [[ "$(lightdark-status)" = "light" ]]; then
        if [[ -n "$ZSH_HIGHLIGHT_STYLES" ]]; then
            ZSH_HIGHLIGHT_STYLES[command]="fg=green"
            ZSH_HIGHLIGHT_STYLES[bracket-level-2]="fg=green"
            ZSH_HIGHLIGHT_STYLES[bracket-level-4]="fg=yellow"
            ZSH_HIGHLIGHT_STYLES[builtin]="fg=green"
            ZSH_HIGHLIGHT_STYLES[function]="fg=green"
            ZSH_HIGHLIGHT_STYLES[alias]="fg=green"
            ZSH_HIGHLIGHT_STYLES[assign]="fg=blue"
            ZSH_HIGHLIGHT_STYLES[double-quoted-argument]="fg=red"
            ZSH_HIGHLIGHT_STYLES[single-quoted-argument]="fg=red,bold"
        fi
        MEGAPROMPT_STYLES[git_ahead_mark]="%b%F{grey}▲%F{cyan}"
        MEGAPROMPT_STYLES[git_behind_mark]="%b%F{grey}▼%F{cyan}"
        MEGAPROMPT_STYLES[git_no_remote_tracking_mark]=" %b%F{grey}N"
        MEGAPROMPT_GIT_STYLES[master]="%b%F{grey}"
    fi
}
hooks-add-hook zle_line_init_hook updateColorStyle

foreground(){
    fg
    mp-updatePrompt
}; zle -N foreground

ls-widget(){
    # echo so the output starts on a fresh line
    echo ""
    ls --color=auto
    # echo a blank line so the last line of output isn't clobbered
    # by my two line prompt
    echo ""
    mp-updatePrompt
}; zle -N ls-widget

insert-last-word-left-indexed(){
    # like the built-in insert-last-word, but if given a numeric argument
    # it chooses the Nth word from the left, rather than the right
    local index
    index="$"
    if [[ -n "$NUMERIC" ]]; then
        index="$NUMERIC"
    fi
    LBUFFER="${LBUFFER}!!${index}"
    zle complete-word
}; zle -N insert-last-word-left-indexed

toggle-konsole-theme(){
    if [[ "light" = "$CURRENT_KONSOLE_THEME" ]]; then
        printf "\033]50;colors=%s\a" "wgh"
        CURRENT_KONSOLE_THEME=dark
    else
        printf "\033]50;colors=%s\a" "wgh-light"
        CURRENT_KONSOLE_THEME=light
    fi
}; zle -N toggle-konsole-theme

bindkey -M viins '^s' backward-kill-word
bindkey -M viins '^[h' completionkey
bindkey-to-prefix-map completionkey "l" insert-last-word-left-indexed
bindkey-to-prefix-map completionkey "L" insert-last-word
bindkey -M emacs '\el' ls-widget
bindkey -M emacs '^Z' foreground
bindkey -M vicmd 'gp' xclip-to-zsh
# include history substring search commands
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey -M vicmd '/' search-forward-current-command
bindkey -M vicmd '?' search-backward-current-command
bindkey -M vicmd 'n' search-forward-current-command-repeat
bindkey -M vicmd 'N' search-backward-current-command-repeat
bindToMaps '^[c' zaw-widgets $(bindkey -l)
bindkey -r -M viins '\e'
bindkey -M viins 'jk' vi-cmd-mode
bindkey -M viins 'kj' vi-cmd-mode
# o/e keys
bindkey -r -M vicmd 'o'
bindkey -r -M vicmd 'e'
bindkey -M vicmd 'ec' forward-char
bindkey -M vicmd 'oc' backward-char
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
bindkey -M vicmd 'tsc' compinit-widget
bindkey -M vicmd 'tsl' toggle-konsole-theme
define-prefix-command spacemap
bindkey -M viins '^@' zaw
bindkey -M viins '^[ ' spacemap
bindkey-to-prefix-map spacemap '' zaw
bindkey-to-prefix-map spacemap "r" zaw-history
bindkey-to-prefix-map spacemap "t" zaw-tmux
bindkey-to-prefix-map spacemap "a" zaw-searcher # ag
bindkey-to-prefix-map spacemap "f" zaw-git-files
bindkey-to-prefix-map spacemap "s" zaw-git-status
bindkey-to-prefix-map spacemap "l" zaw-git-log
bindkey-to-prefix-map spacemap "b" zaw-git-branches
bindkey-to-prefix-map spacemap "o" zaw-open-file
bindkey-to-prefix-map spacemap "p" zaw-process
bindkey-to-prefix-map spacemap "c" zaw-cdr
bindkey-to-prefix-map spacemap "F" zaw-fasd-files
bindkey-to-prefix-map spacemap "w" zaw-widgets
bindkey-to-prefix-map spacemap "-" zaw-widgets
# filter-select is the zaw selection mode
# enable smart-case, ^$ for begin/end, and ! to ignore the next word
zstyle ':filter-select' extended-search yes
zstyle ':filter-select' max-lines 15

bindkey -M filterselect "^[^h" backward-delete-word
bindkey -M filterselect "^[^?" backward-delete-word
# for some reason these don't work...
bindkey -M filterselect "^[b" backward-word
bindkey -M filterselect "^[f" backward-word
bindkey -M filterselect "^[d" kill-word

if [[ -z "$USER" ]]; then
    USER="$(whoami)"
fi
if [[ "$USER" != "wgh" ]]; then
    MEGAPROMPT_STYLES[username]="%B%F{cyan}"
    MEGAPROMPT_DISPLAY_P[username]=true
else
    MEGAPROMPT_DISPLAY_P[username]=false
fi
if [ -z "$SSH_CLIENT" -a -z "$TMUX" -a -n "$DISPLAY" ]; then
    MEGAPROMPT_DISPLAY_P[host]=false
fi
# also, even with no display I don't want it in my Android chroot either...
if [ -n "$ANDROID_CHROOT" -a -z "$SSH_CLIENT" -a -z "$TMUX" ]; then
    MEGAPROMPT_DISPLAY_P[host]=false
fi
# a pox on systems that don't have real hostnames
if [ "$HOST" = localhost -a -z "$SSH_CLIENT" -a -z "$TMUX" ]; then
    MEGAPROMPT_DISPLAY_P[host]=false
fi
MEGAPROMPT_DISPLAY_P[tty]=false
prompt-dev-environment(){
    if [[ -n "$CURRENT_DEV_MODE" ]]; then
        echo -e "Dev: \033[35m$CURRENT_DEV_MODE\n\033[0m"
    fi
}
MEGAPROMPT_PRE_FUNCTION=prompt-dev-environment

if [[ -n "$CURRENT_DEV_MODE" ]]; then
    # In my NixOS racket dev environment, a non-ascii character here messes
    # up the buffer when I use completion
    MEGAPROMPT_STYLES[prompt_char]=":"
fi


if [[ "$HOME" = /data/data/com.termux/files/home ]]; then
    # zsh on termux lacks the pcre module
    MEGAPROMPT_DISPLAY_P[branch_style_regex]=false
    PAGER=less
fi

unfunction compinit
unfunction compdef
autoload -Uz compinit
${=COMPINIT_COMMAND}

if [[ -z "$KONSOLE_DBUS_SESSION" ]]; then
    # currently some programs use the existance of this variable to know
    # whether to use 24-bit color, even though nearly every terminal supports
    # it, and with the same codes.  This should be removed once that's not
    # the case.
    export KONSOLE_DBUS_SESSION=/dev/null
fi

tf=$WGHHOME/rootgit-dotfiles/env.sh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$WGHHOME/rootgit-dotfiles/bazshrc
if [ -f "$tf" ]
then
    source $tf
fi
tf=$WGHHOME/rootgit-dotfiles/zshrc
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

