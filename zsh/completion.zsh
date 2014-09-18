#!/usr/bin/env zsh

zstyle :compinstall filename '$HOME/.zshrc'

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
#zstyle ':completion:*:approximate:*' max-errors 2
# or to have a better heuristic, by allowing one error per 3 character typed
zstyle ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

# let's complete known hosts and hosts from ssh's known_hosts file
#mybasehosts="examplehost.exampledomain.net"
#myhosts=($((
#( [ -r $HOME/.ssh/known_hosts ] && awk '{print $1}' $HOME/.ssh/known_hosts | tr , '\n');\
#echo $mybasehost; ) | sort -u) )

#zstyle ':completion:*' hosts $myhosts

# add partial completion highlighting
zstyle -e ':completion:*:default' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==36=01;31}:${(s.:.)LS_COLORS}")'


function wgh-complete(){
    # match on the left
    # smart-case...ish... and match numbers without using L3-shift (my layout has numbers on AltGr+these letters), and -/_
    zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z} m:{b,m,w,v,h,t,n,g,c,r,-}={0,1,2,3,4,5,6,7,8,9,_}'
    zle expand-or-complete
}
zle -N wgh-complete

function wgh-complete-anywhere(){
    # match anywhere in the potential completion
    zstyle ':completion:*'  matcher-list 'm:{a-z}={A-Z} m:{b,m,w,v,h,t,n,g,c,r,-}={0,1,2,3,4,5,6,7,8,9,_} l:|=* r:|=*'
    zle expand-or-complete
}
zle -N wgh-complete-anywhere


autoload -Uz compinit
compinit

