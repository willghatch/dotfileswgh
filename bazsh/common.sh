#!/bin/bash
# This should never be run... but I'm putting the shabang so that editors will
# recognize it...

export HROOT=$HOME/hroot
export DOTFILESDIR=$HOME/dotfileswgh
export DOTFILESLOCALDIR=$HOME/.dotlocal
PATH=$HOME/bin:$HOME/lscripts:$HOME/vscripts:$HOME/wghsrv_scripts:$HOME/.cabal/bin:$HROOT/bin:$HROOT/usr/bin:$HROOT/usr/local/bin:$PATH:/usr/bin:/bin:/usr/local/bin:/sbin:/usr/sbin:/usr/local/sbin:$DOTFILESDIR/commands
export MANPATH=/usr/share/man:/usr/local/man:/usr/local/share/man:/usr/X11R6/man:/opt/man:$HROOT/usr/share/man:$HROOT/share/man


## Use a default width of 80 for manpages for more convenient reading
export MANWIDTH=${MANWIDTH:-80}

################################################  ALIASES ############################### 


#alias n="mpcl next"
#alias p="mpcl previous"
#alias t="mpcl toggle"

####### Editing
alias em="TERM=xterm-256color emacs -nw -l $HOME/dotfileswgh/emacs/nogui.el"
alias emx="emacs -l $HOME/dotfileswgh/emacs/xgui"
alias emp="TERM=xterm-256color emacs -nw -l $HOME/dotfileswgh/emacs/wghconf-package.el"


####### Unix tools, mostly coloring
alias ls="ls --color=auto"
alias d="ls --color=auto"
alias ll="ls --color=auto -aF"
alias lls="ls --color=auto -laFh"
alias di="ls --color=auto -laFh"
alias pg="less"
alias xterm="xterm -fg white -bg black"
alias textprint="lp -o cpi=15 -o lpi=9 -o page-top=72 -o page-right=72 -o page-left=72 -o page-bottom=72 "
alias cp="cp -a" #archive mode, to preserve timestamps


####### Miscellaneous
alias ffx="env GTK2_RC_FILES=$HOME/dotfileswgh/gtkrc-2.0-light firefox"
alias ffxd="env GTK2_RC_FILES=$HOME/dotfileswgh/gtkrc-2.0-dark firefox"
alias bbb="env GTK2_RC_FILES=$HOME/dotfileswgh/gtkrc-2.0-light dwb"
alias bbbd="env GTK2_RC_FILES=$HOME/dotfileswgh/gtkrc-2.0-dark dwb"

alias gka="gitk --all"
alias gru="git remote update"
alias greb="git rebase"
alias gch="git checkout"
alias gchb="git checkout --branch"
alias grurp="git remote update && git rebase && git push"

alias wdl="wd ls"

alias tm="tmux"
alias tma="tmux attach-session -t"
alias tmad="tmux attach-session -d -t"
alias tml="tmux list-sessions"
alias tmr="tmux rename-session"
alias tmrw="tmux rename-window"
alias tmd="tmux detach"
function tmn()
{
    if test -n "$TMUX"
    then
        pushd ~
        TMUX="" tmux new-session -d -s $@
        tmux switch-client -t $@
        popd
    else
        tmux new-session -s $@
    fi
}

################################ Environment Variables ##################################

export XDG_DOWNLOAD_DIR="$HOME/dl"


export EDITOR=vim
export XTERM=xterm
export PAGER=less
export TERMEM=lxterminal
export TERMEM2=xfce4-terminal


# For color in less (for man pages)

export LESS_TERMCAP_mb=$(printf "\e[1;31m")
export LESS_TERMCAP_md=$(printf "\e[1;31m")
export LESS_TERMCAP_me=$(printf "\e[0m")
export LESS_TERMCAP_se=$(printf "\e[0m")
export LESS_TERMCAP_so=$(printf "\e[1;44;33m")
export LESS_TERMCAP_ue=$(printf "\e[0m")
export LESS_TERMCAP_us=$(printf "\e[1;32m")


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


####### Add shell completion #######

if [ -f /etc/bash_completion.d/git ]
then
    source /etc/bash_completion.d/git
fi

if [ -f /usr/share/git/completion/git-prompt.sh ]
then
    source /usr/share/git/completion/git-prompt.sh
fi

if [ $(uname) = FreeBSD ]
then
    source $DOTFILESDIR/bazsh/bsd.sh
fi

######## Add private stuff #######
bazsh_common_pri=$HOME/dotfileswgh_pri/bazsh_common
if [ -f $bazsh_common_pri ]
then
    source $bazsh_common_pri
fi

