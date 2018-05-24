# $WGHHOME is set so that I can su to other accounts and keep using my settings
if [ -z "$WGHHOME" ]; then
    export WGHHOME=$HOME
fi
export DOTFILESWGH=$WGHHOME/dotfileswgh
source $DOTFILESWGH/env-more.sh

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

if /system/bin/true >/dev/null 2>&1; then
    # I'm on android... bleh...
    export ANDROID_TERMUX_MODE=true
fi

export GUIX_PROFILE=$WGHHOME/.guix-profile
export GUIX_LOCPATH=$GUIX_PROFILE/lib/locale
if [[ "$USE_GUIX" = "true" && -f "$GUIX_PROFILE/etc/profile" ]]; then
    source "$GUIX_PROFILE/etc/profile"
fi
unset MANPATH # so I can use manpath command at end here
export MANPATH=/usr/share/man:/usr/local/man:/usr/local/share/man:/usr/X11R6/man:/opt/man:$HROOT/usr/share/man:$HROOT/share/man:$NPM_DIR/share/man:$(manpath)

# Use my custom locale if it's installed (must be done as root...).
# en_US has a dumb date string (not YYYY-MM-DD), but ISO 8601 (and locales
# that use it, eg en_DK) uses Monday as the first day of the week.
# Also, en_US uses imperial units while I want metric, and mine
# customizes telephone layouts to display as NNN-NNN-NNNN rather than
# (NNN) NNN-NNNN... though I don't know where that even comes up in 
# software that uses locale settings...
#if [[ -f /usr/share/i18n/locales/en_US@willghatch ]]; then
#    # maybe I should test with `localectl list-locales`, except I'll have to grep as well.
#    export LANG=en_US@willghatch
#else
#    export LANG=en_US.utf8
#fi

# pacaur alias to stop it from fighting with guix...
alias pacaur="env EDITOR=vim PATH=/bin:/bin/site_perl:/bin/vendor_perl:/bin/core_perl LIBRARY_PATH= pacaur"

# I'll leave these commented for now.  I exported these to build stuff in my
# $HROOT, but I'm hoping something like guix solves my user packaging issues...
#export PKG_CONFIG_PATH=$HROOT/lib/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig:/lib/pkgconfig
#export LD_LIBRARY_PATH=$HROOT/lib:/usr/local/lib:/usr/lib:/lib

if [[ "$ANDROID_TERMUX_MODE" ]]; then
    # Set LD_LIBRARY_PATH for termux
    LD_LIBRARY_PATH=$PREFIX/lib:$LD_LIBRARY_PATH
fi

XDG_RUNTIME_DIR="/run/user/$(id -u)"
if [[ ! -d "$XDG_RUNTIME_DIR" ]]; then
    XDG_RUNTIME_DIR="/tmp/${USER}_$(id -u)/runtime"
    mkdir -p "$XDG_RUNTIME_DIR"
    chmod 700 "$XDG_RUNTIME_DIR"
fi
export XDG_RUNTIME_DIR


# themes to ease eye strain
if [[ "$WGH_THEME_DARK_OR_LIGHT" = "light" ]]; then
    export WGH_THEME_DARK_OR_LIGHT="light"
    export GTK_THEME=Adwaita:light
    export GTK2_RC_FILES=$DOTFILESWGH/gtk2rc-light
else
    export WGH_THEME_DARK_OR_LIGHT="dark"
    export GTK_THEME=Adwaita:dark
    export GTK2_RC_FILES=$DOTFILESWGH/gtk2rc-dark
fi

# For color in less (for man pages)
export LESS_TERMCAP_mb=$(printf "\e[1;34m")
export LESS_TERMCAP_md=$(printf "\e[1;31m")
export LESS_TERMCAP_me=$(printf "\e[0m")
export LESS_TERMCAP_se=$(printf "\e[0m")
export LESS_TERMCAP_so=$(printf "\e[1;44;33m")
export LESS_TERMCAP_ue=$(printf "\e[0m")
# this is supposed to be italic
export LESS_TERMCAP_us=$(printf "\e[1;3;32m")

umask 022 # the covenient umask

################################################  ALIASES ###############################

alias rr="racket -l rash/repl --"



####### Unix tools, mostly coloring
alias ls="ls --color=auto"
alias d="ls --color=auto"
alias lg="ls | grep -i"
alias ll="ls --color=auto -aF"
alias lls="ls --color=auto -laFh"
alias di="ls --color=auto -laFh"
alias igrep="grep -i"
alias pg="less"
alias textprint="lp -o cpi=15 -o lpi=9 -o page-top=72 -o page-right=72 -o page-left=72 -o page-bottom=72 "
alias cp="cp -a -i" # archive mode, to preserve timestamps, interactive check when overwriting
alias mv="mv -i" # ask before overwriting something else
alias ag="ag --color-match '1;36'"
alias ags="ag --color-match '1;36' --smart-case"
alias pyg="pygmentize -g --" # colored cat, with pygments installed with pip
alias feh="feh --scale-down" # think this is always the default I want...
alias atcat="at -c"
alias o="xdg-open"

alias make="time nice make" # this is always what I actually want
# parallel make
export NUMCPUS=$(grep -c '^processor' /proc/cpuinfo 2>/dev/null || echo 2)
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

alias arx="archive-command extract"
alias arl="archive-command list"
alias arc="archive-command create"

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

for-each-line(){
    local line
    while read line; do
        "$@" "$line"
    done
}

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
tic -x $DOTFILESWGH/xterm-256color-italic.terminfo
tic -x $DOTFILESWGH/xterm-24bit.terminfo
tic -x $DOTFILESWGH/xterm-24bit-italic.terminfo
if [[ "$TERM" = "xterm" || "$TERM" = "xterm-256color" ]]; then
    TERM=xterm-24bit-italic
fi

lesskey $DOTFILESWGH/lesskey 2>/dev/null

source $DOTFILESWGH/bazsh/grml-funcs.sh

UNAME=$(uname)
if [[ $UNAME = FreeBSD || $UNAME = Darwin ]]
then
    source $DOTFILESWGH/bazsh/bsd.sh
fi

######## Add private stuff #######
bazsh_common_pri=$DOTFILESWGH/pri/bazshrc
if [ -f $bazsh_common_pri ]
then
    source $bazsh_common_pri
fi

bazsh_common_rootgit=$WGHHOME/rootgit-dotfiles/bazshrc
if [ -f $bazsh_common_rootgit ]
then
    source $bazsh_common_rootgit
fi

bazsh_common_local=$DOTFILESWGH/dotlocal/bazshrc
if [ -f $bazsh_common_local ]
then
    source $bazsh_common_local
fi

bazsh_common_pri_local=$DOTFILESWGH/pri/dotlocal/bazshrc
if [ -f $bazsh_common_pri_local ]
then
    source $bazsh_common_pri_local
fi

