#!/bin/bash
# If not running interactively, don't do anything
[ -z "$PS1" ] && return


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

if [[ -z "$WGHHOME" ]]; then
    export WGHHOME=$HOME # I want to be able to reference this, and when it's
    #not true(IE when I use su), I'll set this elsewhere
fi

bazsh_common=$WGHHOME/dotfileswgh/bazsh/common.sh
if [ -f $bazsh_common ]; then
      . $bazsh_common
fi


umask 027

# Turn off TTY "start" and "stop" commands in all interactive shells.
# They default to C-q and C-s, Bash uses C-s to do a forward history search.
stty start ''
stty stop  ''
stty -ixon # disable XON/XOFF flow control
stty ixoff # enable sending (to app) of start/stop characters
stty ixany # let any character restart output, not only start character



export PS1="\[$(tput bold)\]\[$(tput setaf 6)\]\A \[$(tput setaf 7)\][\[$(tput setaf 2)\]\u\[$(tput setaf 7)\]@\[$(tput setaf 4)\]\h\[$(tput setaf 7)\]] \[$(tput setaf 4)\]\w \[$(tput setaf 7)\]\\$\[$(tput sgr0)\] "


sourceIfExists $DOTFILESWGH/dotlocal/bazshrc

sourceIfExists $DOTFILESWGH/dotlocal/bashrc


