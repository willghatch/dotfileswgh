#!/usr/bin/zsh
HISTFILE=~/.zsh.history
HISTSIZE=1000
SAVEHIST=1000
bindkey -e

bazsh_common=$HOME/dotfileswgh/bazsh_common
if [ -f $bazsh_common ]
then
    source $bazsh_common
fi

fpath=($DOTFILESDIR/external/zsh/wd $fpath)

zstyle :compinstall filename '$HOME/.zshrc'
# smart-case...ish...
#zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z}'
# smart-case...ish... and match numbers without using L3-shift (my layout has numbers on AltGr+these letters), and -/_
#zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z} m:{b,m,w,v,h,t,n,g,c,r,-}={0,1,2,3,4,5,6,7,8,9,_} l:|=* r:|=*'
# for now, don't match in the middle, just on the left...
zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z} m:{b,m,w,v,h,t,n,g,c,r,-}={0,1,2,3,4,5,6,7,8,9,_}'

# format all messages not formatted in bold prefixed with ----
zstyle ':completion:*' format '%B---- %d%b'
# format descriptions (notice the vt100 escapes)
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'
# bold and underline normal messages
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
# format in bold red error messages
zstyle ':completion:*:warnings' format "%B$fg[red]%}---- no match for: $fg[white]%d%b"
# Put completion into groups
zstyle ':completion:*' group-name ''

# activate approximate completion, but only after regular completion (_complete)
zstyle ':completion:::::' completer _complete _approximate
# limit to 2 errors
zstyle ':completion:*:approximate:*' max-errors 2
# or to have a better heuristic, by allowing one error per 3 character typed
# zstyle ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# let's complete known hosts and hosts from ssh's known_hosts file
#mybasehosts="examplehost.exampledomain.net"
#myhosts=($((
#( [ -r $HOME/.ssh/known_hosts ] && awk '{print $1}' $HOME/.ssh/known_hosts | tr , '\n');\
#echo $mybasehost; ) | sort -u) )

#zstyle ':completion:*' hosts $myhosts

# add partial completion highlighting
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==36=01;31}:${(s.:.)LS_COLORS}")'

autoload -Uz compinit
compinit


autoload -U colors && colors

setopt PROMPT_SUBST

umask 077

# no c-s/c-q output freezing
setopt noflowcontrol

# this is default, but set for share_history
setopt append_history
# import new commands from the history file also in other zsh-session
#setopt share_history
# save each command's beginning timestamp and the duration to the history file
setopt extended_history
# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list
#setopt histignorealldups
# remove command lines from the history list when the first character on the
# line is a space
#setopt histignorespace
# if a command is issued that can't be executed as a normal command, and the
# command is the name of a directory, perform the cd command to that directory.
#setopt auto_cd
# in order to use #, ~ and ^ for filename generation grep word
# *~(*.gz|*.bz|*.bz2|*.zip|*.Z) -> searches for word not in compressed files
# don't forget to quote '^', '~' and '#'!
setopt extended_glob
# display PID when suspending processes as well
setopt longlistjobs
# try to avoid the 'zsh: no matches found...'
setopt nonomatch
# report the status of backgrounds jobs immediately
setopt notify
# whenever a command completion is attempted, make sure the entire command path
# is hashed first.
setopt hash_list_all
# not just at the end
setopt completeinword
# Don't send SIGHUP to background processes when the shell exits.
#setopt nohup
# make cd push the old directory onto the directory stack.
#setopt auto_pushd
# avoid "beep"ing
#setopt nobeep
# don't push the same dir twice.
#setopt pushd_ignore_dups
# * shouldn't match dotfiles. ever.
#setopt noglobdots
# use zsh style word splitting
setopt noshwordsplit
# don't error out when unset parameters are used
setopt unset


# mailchecks
#MAILCHECK=30
# report about cpu-/system-/user-time of command if running longer than this
REPORTTIME=5
# watch for everyone but me and root
#watch=(notme root)
# automatically remove duplicates from these arrays
typeset -U path cdpath fpath manpath



PS1_time='%F{cyan}%T'
PS1_user_host='%F{green}[%B%n%b%F{yellow}@%B%m%b%F{green}]'
PS1_dir='%F{blue}%B%~'
PS1_cmd_stat='%b%F{cyan}<%F{red}%?%F{cyan}>'
PS1_end='%F{default}
%# '
PS1_batt_state="\$(batt_state.bash)" 

#PS1="%T [%n@%m] %~ %?%# "
#PS1='%F{cyan}%T %F{green}[%B%n%b%F{yellow}@%B%m%b%F{green}] %F{blue}%B%~ %b%F{cyan}<%F{red}%?%F{cyan}>%F{default}%#
#'
PS1="${PS1_time} ${PS1_user_host} ${PS1_dir} ${PS1_cmd_stat}${PS1_end}"



source $DOTFILESDIR/zsh/zshkeys-pre

wd() {
    source $DOTFILESDIR/external/zsh/wd/wd.sh
}

sourceIfExists $DOTFILESDIR/zsh/abbreviations.zsh

tf=$DOTFILESDIR/zsh/grml-funcs.zsh
if [ -f "$tf" ]
then
    source $tf
fi

tf=$DOTFILESDIR/external/zsh/opp.zsh/opp.zsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESDIR/external/zsh/opp.zsh/opp/textobj-between.zsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESDIR/external/zsh/opp.zsh/opp/surround.zsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESDIR/external/zsh/zle_vi_visual.zsh
if [ -f "$tf" ]
then
    source $tf
fi

# Highlighter plugin config
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)
tf=$DOTFILESDIR/external/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [ -f "$tf" ]
then
    source $tf
fi
# Highlighters main
ZSH_HIGHLIGHT_STYLES[default]="none"
ZSH_HIGHLIGHT_STYLES[unknown-token]="fg=red,bold"
ZSH_HIGHLIGHT_STYLES[path]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[path_approx]="fg=cyan,underline"
ZSH_HIGHLIGHT_STYLES[path_prefix]="fg=cyan"
ZSH_HIGHLIGHT_STYLES[globbing]="fg=blue,bold,strikethrough"
ZSH_HIGHLIGHT_STYLES[reserved-word]="fg=yellow"
ZSH_HIGHLIGHT_STYLES[history-expansion]="fg=blue,bold"
ZSH_HIGHLIGHT_STYLES[commandseparator]="fg=yellow,bold"
ZSH_HIGHLIGHT_STYLES[precommand]="fg=green"
ZSH_HIGHLIGHT_STYLES[command]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[hashed-command]="fg=green,underline"
ZSH_HIGHLIGHT_STYLES[builtin]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[function]="fg=green,bold"
ZSH_HIGHLIGHT_STYLES[alias]="fg=green,bold,underline"
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



tf=$DOTFILESDIR/external/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
if [ -f "$tf" ]
then
    source $tf
fi

source $DOTFILESDIR/zsh/zshkeys-post

tf=$DOTFILESLOCALDIR/bazsh
if [ -f "$tf" ]
then
    source $tf
fi
tf=$DOTFILESLOCALDIR/zshrc
if [ -f "$tf" ]
then
    source $tf
fi

if [ -x ~/vscripts/motd.sh ]
then
    ~/vscripts/motd.sh
fi

