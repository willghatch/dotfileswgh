#!/bin/bash
# If not running interactively, don't do anything
#[ -z "$PS1" ] && return
if [[ "${-#*i}" = "$-" ]]; then
    return;
fi


#-------------------------------------------------------------
# Source global definitions (if any)
#-------------------------------------------------------------
if [ -f /etc/bashrc ]; then
      . /etc/bashrc   # --> Read /etc/bashrc, if present.
fi
if [ -f /etc/bash.bashrc ]; then
      . /etc/bash.bashrc   # --> Read /etc/bash.bashrc, if present.
fi

if [ -f ~/.bazshrc.pre ]; then
      . ~/.bazshrc.pre
fi


### detect dotfileswgh snippet
if [ -z "$DOTFILESWGH" ]; then
    if [ -d "$HOME/dotfileswgh" ]; then
        export DOTFILESWGH="$HOME/dotfileswgh"
    elif [ -d /rootgit/base.rootgit/dotfileswgh ]; then
        export DOTFILESWGH=/rootgit/base.rootgit/dotfileswgh
    fi
fi
export DOTFILESWGH="${DOTFILESWGH:-$HOME/dotfileswgh}"
###

. $DOTFILESWGH/bazshrc


# Turn off TTY "start" and "stop" commands in all interactive shells.
# They default to C-q and C-s, Bash uses C-s to do a forward history search.
stty start ''
stty stop  ''
stty -ixon # disable XON/XOFF flow control
stty ixoff # enable sending (to app) of start/stop characters
stty ixany # let any character restart output, not only start character


pcolor(){
    echo -ne "\[\e[${1}m\]"
}

#export PS1="\[$(tput bold)\]\[$(tput setaf 6)\]\A \[$(tput setaf 7)\][\[$(tput setaf 2)\]\u\[$(tput setaf 7)\]@\[$(tput setaf 4)\]\h\[$(tput setaf 7)\]] \[$(tput setaf 4)\]\w \[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "
PS1_time="$(pcolor 0)$(pcolor 1)$(pcolor 31)\A"
PS1_userhost="$(pcolor 0)[$(pcolor 32)\u$(pcolor 0)@$(pcolor 36)\h$(pcolor 0)]"
PS1_dir="$(pcolor 0)$(pcolor 1)$(pcolor 34)\w"
PS1_promptchar="$(pcolor 0)\$"
export PS1="$PS1_time $PS1_userhost $PS1_dir $PS1_promptchar "
if test -n "$CURRENT_DEV_MODE"; then
    PS1="\033[42;31mDev\033[0m: \033[38;5;200m$CURRENT_DEV_MODE \033[0m\w/\n\$ "
fi
if test -n "$DOCSUEZ_ENV_NAME"; then
    PS1="\033[42;31mDocsuez\033[0m: \033[38;5;200m$DOCSUEZ_ENV_NAME \033[0m\w/\n\$ "
fi


sourceFromDotfileswghAlts env.sh
sourceFromDotfileswghAlts bashrc


