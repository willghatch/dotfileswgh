#!/usr/bin/env zsh

zstyle :compinstall filename $COMPINSTALL_FILE

############### NOTE here -
# I don't think I really understand the right way to configure zsh completers, but I'm sick of trying
# to figure it out from the docs, so I'll just use my own workarounds here to have the settings I want
# on different completion commands

# This function sets all of my default completion options, then the other completion/
# matching functions can reset any of them, then reset to std
function __zs-std(){
    # match on the left
    # smart-case...ish... and match numbers without using L3-shift (my layout has numbers on AltGr+these letters), and -/_
    zstyle ':completion:*'  matcher-list 'm:{a-z-}={A-Z_} '
    zstyle ':completion:::::' completer _complete _approximate

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

    if [[ -n "$COMPLETER_DEFAULT_SETUP" ]]
    then
        $COMPLETER_DEFAULT_SETUP
    fi
}
__zs-std # call this one immediately as it's the default

function __zs-matcher-anywhere(){
    # match anywhere in the potential completion
    zstyle ':completion:*'  matcher-list 'm:{a-z-}={A-Z_} m:{b,m,w,v,h,t,n,g,c,r}={0,1,2,3,4,5,6,7,8,9} l:|=* r:|=*'
}
function __zs-completers-gnu(){
    zstyle ':completion:::::' completer _complete _gnu_generic
}
function __zs-completers-expand(){
    zstyle ':completion:::::' completer _expand_word _expand_alias
}
function __zs-completers-history(){
    zstyle ':completion:::::' completer _history
}
function __zs-completers-tmux(){
    zstyle ':completion:::::' completer _complete_tmux_display
}
function __zs-completers-maximal(){
    zstyle ':completion:::::' completer _oldlist _expand _complete _files _ignored _history _complete_tmux_display _gnu_generic _prefix _match _approximate
}
# _all_matches _approximate _complete _correct _expand _expand_alias _extensions
# _history _ignored _list _match _menu _oldlist _prefix _user_expand
# list from http://zsh.sourceforge.net/Doc/Release/Completion-System.html
# In the bindable commands section they have something about _generic that
# seems to be the way to set :completion:foo:... contexts that I tried so hard
# to figure out before... maybe I'll look into that more later.


function mk-completer(){
    # mk-completer <config-func> <new-func-name>
    eval "
        function $2(){
            $1
            zle complete-word
            __zs-std
        }
    "
    zle -N $2
}

mk-completer __zs-std complete-std
mk-completer __zs-matcher-anywhere complete-std-anywhere
mk-completer __zs-completers-gnu complete-gnu
mk-completer __zs-completers-expand complete-expand
mk-completer __zs-completers-history complete-history
mk-completer __zs-completers-history-anywhere complete-history-anywhere
mk-completer __zs-completers-tmux complete-tmux
mk-completer __zs-completers-maximal complete-maximal
mk-completer __zs-completers-maximal-anywhere complete-maximal-anywhere

autoload -Uz compinit
compinit

