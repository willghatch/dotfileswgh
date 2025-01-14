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
export DOTFILESWGH=${DOTFILESWGH:-$WGHHOME/dotfileswgh}

bazsh_common=$DOTFILESWGH/bazshrc
if [ -f $bazsh_common ]; then
      . $bazsh_common
fi



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
    PS1="\033[42;31mDev\033[0m: \033[35m$CURRENT_DEV_MODE \033[0m\w/\n\$ "
fi


sourceIfExists $WGHHOME/rootgit-dotfiles/env.sh
sourceIfExists $WGHHOME/rootgit-dotfiles/bazshrc
sourceIfExists $WGHHOME/rootgit-dotfiles/bashrc
sourceIfExists $DOTFILESWGH/dotlocal/bashrc


