#!/usr/bin/zsh
HISTFILE=~/.zsh.history
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
zstyle :compinstall filename '$HOME/.zshrc'
# smart-case...ish...
#zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z}'
# smart-case...ish... and match numbers without using L3-shift (my layout has numbers on AltGr+these letters), and -/_
zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z} m:{b,m,w,v,h,t,n,g,c,r,-}={0,1,2,3,4,5,6,7,8,9,_} l:|=* r:|=*'

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

bazsh_common=$HOME/dotfileswgh/bazsh_common
if [ -f $bazsh_common ]
then
    source $bazsh_common
fi

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


if [ -f $DOTFILESLOCALDIR/bazsh ]
then
    source $DOTFILESLOCALDIR/bazsh
fi

if [ -f $DOTFILESLOCALDIR/zshrc ]
then
    source $DOTFILESLOCALDIR/zshrc
fi

if [ -x ~/vscripts/motd.sh ]
then
    ~/vscripts/motd.sh
fi

