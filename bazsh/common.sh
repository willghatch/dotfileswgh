# $WGHHOME is set so that I can su to other accounts and keep using my settings
if [ -z "$WGHHOME" ]; then
    export WGHHOME=$HOME
fi
export DOTFILESWGH=$WGHHOME/dotfileswgh

# When $WGHHOME is not equal to $HOME I've switched to another user, and while I'll
# want to keep using my normal dotfiles and such for most things, some settings will
# need to change.
i-am-me() {
    if [[ "$HOME" = "$WGHHOME" ]]; then
        return 0
    else
        return 1
    fi
}

export HROOT=$WGHHOME/hroot
export NPM_DIR=$DOTFILESWGH/dotlocal/npm
export NODE_PATH="$NPM_DIR/lib/node_modules"
PATH=$DOTFILESWGH/dotlocal/commands:$DOTFILESWGH/commands:$DOTFILESWGH/external/misc/rootgit-tools:$WGHHOME/.cabal/bin:$NPM_DIR/bin:$HROOT/bin:$HROOT/usr/bin:$HROOT/usr/local/bin:$WGHHOME/.local/bin:$WGHHOME/bin:$PATH:/usr/bin:/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:$WGHHOME/.screenlayout
unset MANPATH # so I can use manpath command at end here
export MANPATH=/usr/share/man:/usr/local/man:/usr/local/share/man:/usr/X11R6/man:/opt/man:$HROOT/usr/share/man:$HROOT/share/man:$NPM_DIR/share/man:$(manpath)

# XDG basedir spec
# I'm putting the defaults all here as reference
# Having the read-only path to search for config and data is *awesome*,
# but unfortunately most things ignore them and only use data/config_home.
#
# writable data location
#XDG_DATA_HOME=$HOME/.local/share
# read-only but searched data locations
#XDG_DATA_DIRS=/usr/local/share/:/usr/share/
XDG_DATA_DIRS=$DOTFILESWGH/local/share/:/usr/local/share/:/usr/share/
# writable config location
#XDG_CONFIG_HOME=$HOME/.config
# read-only but searched config locations
#XDG_CONFIG_DIRS=/etc/xdg
XDG_CONFIG_DIRS=$DOTFILESWGH/config:/etc/xdg
# writable cache dir
#XDG_CACHE_HOME=$HOME/.cache

# themes to ease eye strain
export GTK_THEME=gtk-adb
export GTK2_RC_FILES=$DOTFILESWGH/gtk2rc-dark
export QT_STYLE_OVERRIDE=gtk

## Use a default width of 80 for manpages for more convenient reading
export MANWIDTH=${MANWIDTH:-80}

export ZDOTDIR=$DOTFILESWGH/zsh

# v is a wrapper script to launch vim with my settings
export ALTERNATE_EDITOR=v
if which emacs 1>/dev/null 2>/dev/null && i-am-me; then
    export EDITOR=premacs-use-t
    export PAGER=premacs-pager
else
    export EDITOR=$ALTERNATE_EDITOR
    export PAGER=less
fi

export XDG_DOWNLOAD_DIR="$HOME/dl"
export VLAUNCHRC="$DOTFILESWGH/vlaunchrc"


# For color in less (for man pages)
export LESS_TERMCAP_mb=$(printf "\e[1;34m")
export LESS_TERMCAP_md=$(printf "\e[1;31m")
export LESS_TERMCAP_me=$(printf "\e[0m")
export LESS_TERMCAP_se=$(printf "\e[0m")
export LESS_TERMCAP_so=$(printf "\e[1;44;33m")
export LESS_TERMCAP_ue=$(printf "\e[0m")
# this is supposed to be italic
export LESS_TERMCAP_us=$(printf "\e[1;3;32m")

export GREP_COLOR="1;32"

umask 022 # the covenient umask

################################################  ALIASES ###############################


#alias n="mpcl next"
#alias p="mpcl previous"
#alias t="mpcl toggle"

####### Editing
alias em="emacs -nw -l $DOTFILESWGH/emacs/def"
alias e="$EDITOR"
alias eg="premacs-use -c"
alias emg="emacs -l $DOTFILESWGH/emacs/def"
alias emp="emacs -nw -l $DOTFILESWGH/emacs/package-conf.el"


####### Unix tools, mostly coloring
alias ls="ls --color=auto"
alias d="ls --color=auto"
alias lg="ls | grep -i"
alias ll="ls --color=auto -aF"
alias lls="ls --color=auto -laFh"
alias di="ls --color=auto -laFh"
alias pg="less"
alias textprint="lp -o cpi=15 -o lpi=9 -o page-top=72 -o page-right=72 -o page-left=72 -o page-bottom=72 "
alias cp="cp -a" #archive mode, to preserve timestamps
alias ag="ag --color-match '1;36'"
alias ags="ag --color-match '1;36' --smart-case"
alias pyg="pygmentize -g --" # colored cat, with pygments installed with pip
alias feh="feh --scale-down" # think this is always the default I want...
alias atcat="at -c"
alias o="xdg-open"

alias make="time nice make" # this is always what I actually want
# parallel make
export NUMCPUS=$(grep -c '^processor' /proc/cpuinfo)
alias pmake="time nice make -j$NUMCPUS --load-average=$NUMCPUS"

alias xclipo="xclip -o -selection clipboard"
alias xclipi="xclip -i -selection clipboard"

####### Miscellaneous
alias gka="gitk --all"
alias gta="tig --all"
alias gunadd="git reset HEAD"
# I never use ghost script
alias gs="git status"
alias greb="git rebase"
alias gru="git remote update"
alias grur="git remote update && git rebase"
alias grurp="git remote update && git rebase && git push"
grebi(){
    git rebase -i HEAD~${1}
}
# I never use the gc command
alias gc="git commit"
alias gco="git checkout"
alias gcob="git checkout -b"
alias gclone="git clone --recursive"
alias gp="git push"
alias gd="git diff"
alias ga="git add"

alias hloc="locate --database $HOME/.cache/hloc.db"
alias hupdb="updatedb --output $HOME/.cache/hloc.db --database-root $HOME --require-visibility no --add-prunepaths $HOME/.snapshot"
export LOCATE_PATH=$HOME/.cache/hloc.db

alias tm="tmux -f $DOTFILESWGH/tmux.conf"
alias tma="tmux -f $DOTFILESWGH/tmux.conf attach-session -t"
alias tmad="tmux -f $DOTFILESWGH/tmux.conf attach-session -d -t"
alias tml="tmux -f $DOTFILESWGH/tmux.conf list-sessions"
alias tmr="tmux -f $DOTFILESWGH/tmux.conf rename-session"
alias tmrw="tmux -f $DOTFILESWGH/tmux.conf rename-window"
alias tmd="tmux -f $DOTFILESWGH/tmux.conf detach"
function tmn()
{
    if test -n "$TMUX"
    then
        pushd ~
        TMUX="" tmux -f $DOTFILESWGH/tmux.conf new-session -d -s $@
        tmux -f $DOTFILESWGH/tmux.conf switch-client -t $@
        popd
    else
        tmux -f $DOTFILESWGH/tmux.conf new-session -s $@
    fi
}

c(){
    builtin cd "$@" && \
    ls -aF --color=auto
}

# Run "a" command, which runs a command in the background disowned, then die.
# IE. replace this xterm with this graphical command
ak(){
    a $@
    exit
}

## recent versions of Libre Office in Arch Linux don't work without this, because they
## switched to the upstream default of gtk3, which doesn't work because reasons.
alias libreoffice="env SAL_USE_VCLPLUGIN=gtk libreoffice"


#################################### Functions ###########################################

# Generic Shell Functions

function volume()
{
    if [ -z $1 ]
    then
        volume.sh
    else
        amixer sset Master $1
    fi
}


# to keep myself from killing my crontab...
crontab()
{
    if [ -z $1 ]
    then
        echo "Whoa, better put on a -e or you could kill your crontab."
        echo "That would be bad."
        return 1
    fi

    /usr/bin/crontab $@
}

# xml viewing in pager...
xmlless()
{
    xmllint --format $@ | less
}

# seconds since epoch
epoch()
{
    if [ -n "$1" ]
    then
        date -d "$1" +%s
    else
        date +%s
    fi
}

# for sourcing
sourceIfExists()
{
    for f in $@
    do
        if [ -f $f ]
        then
            source $f
        fi
    done

}

# for web development
cpost()
{
    if [ -z "$1" ]
    then
        echo "convenient curl wrapper for posting"
        echo "usage: $0 <url> [var val] [var val] ... [- <curl options>]"
        return
    else
        url=$1
        shift
        variables=""
        while [ -n "$1" ]
        do
            if [ "$1" = "-" ]
            then
                shift
                break
            else
                varval="$1=$2"
                shift
                shift
                if [ -z "$hasVars" ]
                then
                    variables="$varval"
                else
                    variables="$variables&$varval"
                fi
                hasVars="--data"
            fi
        done
        curl --request POST $url "$hasVars" "$variables" $@
    fi
}

# fix terminal
tic $DOTFILESWGH/xterm-256color-italic.terminfo
if [[ "$TERM" = "xterm" ]]; then
    TERM=xterm-256color-italic
fi

lesskey $DOTFILESWGH/lesskey

source $DOTFILESWGH/bazsh/grml-funcs.sh

if [ $(uname) = FreeBSD ]
then
    source $DOTFILESWGH/bazsh/bsd.sh
fi

######## Add private stuff #######
bazsh_common_pri=$DOTFILESWGH/pri/bazsh_common
if [ -f $bazsh_common_pri ]
then
    source $bazsh_common_pri
fi

